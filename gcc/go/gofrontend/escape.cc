// escape.cc -- Go escape analysis (based on Go compiler algorithm).

// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <limits>
#include <stack>
#include <sstream>

#include "gogo.h"
#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "escape.h"
#include "lex.h"
#include "ast-dump.h"
#include "go-optimize.h"
#include "go-diagnostics.h"
#include "go-sha1.h"

// class Node.

// Return the node's type, if it makes sense for it to have one.

Type*
Node::type() const
{
  if (this->object() != NULL
      && this->object()->is_variable())
    return this->object()->var_value()->type();
  else if (this->object() != NULL
	   && this->object()->is_function())
    return this->object()->func_value()->type();
  else if (this->expr() != NULL)
    return this->expr()->type();
  else if (this->is_indirect())
    {
      if (this->child()->type()->deref()->is_void_type())
        // This is a void*. The referred type can be actually any type,
        // which may also be pointer. We model it as another void*, so
        // we don't lose pointer-ness.
        return this->child()->type();
      else if (this->child()->type()->is_slice_type())
        // We model "indirect" of a slice as dereferencing its pointer
        // field (to get element). Use element type here.
        return this->child()->type()->array_type()->element_type();
      else if (this->child()->type()->is_string_type())
        return Type::lookup_integer_type("uint8");
      else
        return this->child()->type()->deref();
    }
  else if (this->statement() != NULL
           && this->statement()->temporary_statement() != NULL)
    return this->statement()->temporary_statement()->type();
  else
    return NULL;
}

// A helper for reporting; return this node's location.

Location
Node::location() const
{
  if (this->object() != NULL && !this->object()->is_sink())
    return this->object()->location();
  else if (this->expr() != NULL)
    return this->expr()->location();
  else if (this->statement() != NULL)
    return this->statement()->location();
  else if (this->is_indirect())
    return this->child()->location();
  else
    return Linemap::unknown_location();
}

// A helper for reporting; return the location where the underlying
// object is defined.

Location
Node::definition_location() const
{
  if (this->object() != NULL && !this->object()->is_sink())
    {
      Named_object* no = this->object();
      if (no->is_variable() || no->is_result_variable())
        return no->location();
    }
  else if (this->expr() != NULL)
    {
      Var_expression* ve = this->expr()->var_expression();
      if (ve != NULL)
        {
          Named_object* no = ve->named_object();
          if (no->is_variable() || no->is_result_variable())
            return no->location();
        }
      Enclosed_var_expression* eve = this->expr()->enclosed_var_expression();
      if (eve != NULL)
        {
          Named_object* no = eve->variable();
          if (no->is_variable() || no->is_result_variable())
            return no->location();
        }
    }
  return this->location();
}

// To match the cmd/gc debug output, strip away the packed prefixes on functions
// and variable/expressions.

std::string
strip_packed_prefix(Gogo* gogo, const std::string& s)
{
  std::string packed_prefix = "." + gogo->pkgpath() + ".";
  std::string fmt = s;
  for (size_t pos = fmt.find(packed_prefix);
       pos != std::string::npos;
       pos = fmt.find(packed_prefix))
    fmt.erase(pos, packed_prefix.length());
  return fmt;
}

// A helper for debugging; return this node's AST formatted string.
// This is an implementation of gc's Nconv with obj.FmtShort.

std::string
Node::ast_format(Gogo* gogo) const
{
  std::ostringstream ss;
  if (this->is_sink())
    ss << ".sink";
  else if (this->object() != NULL)
    {
      Named_object* no = this->object();
      if (no->is_function() && no->func_value()->enclosing() != NULL)
	return "func literal";
      ss << no->message_name();
    }
  else if (this->expr() != NULL)
    {
      Expression* e = this->expr();
      bool is_call = e->call_expression() != NULL;
      if (is_call)
	e->call_expression()->fn();
      Func_expression* fe = e->func_expression();;

      bool is_closure = fe != NULL && fe->closure() != NULL;
      if (is_closure)
	{
	  if (is_call)
	    return "(func literal)()";
	  return "func literal";
	}
      Ast_dump_context::dump_to_stream(this->expr(), &ss);
    }
  else if (this->statement() != NULL)
    {
      Statement* s = this->statement();
      Goto_unnamed_statement* unnamed = s->goto_unnamed_statement();
      if (unnamed != NULL)
	{
	  Statement* derived = unnamed->unnamed_label()->derived_from();
	  if (derived != NULL)
	    {
	      switch (derived->classification())
		{
		case Statement::STATEMENT_FOR:
		case Statement::STATEMENT_FOR_RANGE:
		  return "for loop";
		  break;

		case Statement::STATEMENT_SWITCH:
		  return "switch";
		  break;

		case Statement::STATEMENT_TYPE_SWITCH:
		  return "type switch";
		  break;

		default:
		  break;
		}
	    }
	}
      Temporary_statement* tmp = s->temporary_statement();
      if (tmp != NULL)
        {
          // Temporary's format can never match gc's output, and
          // temporaries are inserted differently anyway. We just
          // print something convenient.
          ss << "tmp." << (uintptr_t) tmp;
          if (tmp->init() != NULL)
            {
              ss << " [ = ";
              Ast_dump_context::dump_to_stream(tmp->init(), &ss);
              ss << " ]";
            }
        }
      else
        Ast_dump_context::dump_to_stream(s, &ss);
    }
  else if (this->is_indirect())
    return "*(" + this->child()->ast_format(gogo) + ")";

  std::string s = strip_packed_prefix(gogo, ss.str());

  // trim trailing space
  return s.substr(0, s.find_last_not_of(' ') + 1);
}

// A helper for debugging; return this node's detailed format string.
// This is an implementation of gc's Jconv with obj.FmtShort.

std::string
Node::details()
{
  std::stringstream details;

  if (!this->is_sink())
    details << " l(" << Linemap::location_to_line(this->location()) << ")";

  bool is_varargs = false;
  bool is_address_taken = false;
  bool is_in_heap = false;
  bool is_assigned = false;
  std::string class_name;

  Expression* e = this->expr();
  Named_object* node_object = NULL;
  if (this->object() != NULL)
    node_object = this->object();
  else if (e != NULL && e->var_expression() != NULL)
    node_object = e->var_expression()->named_object();

  if (node_object)
    {
      // TODO(cmang): For named variables and functions, we want to output
      // the function depth.
      if (node_object->is_variable())
	{
	  Variable* var = node_object->var_value();
	  is_varargs = var->is_varargs_parameter();
	  is_address_taken = (var->is_address_taken()
			      || var->is_non_escaping_address_taken());
	  is_in_heap = var->is_in_heap();
	  is_assigned = var->init() != NULL;

	  if (var->is_global())
	    class_name = "PEXTERN";
	  else if (var->is_parameter())
	    class_name = "PPARAM";
	  else if (var->is_closure())
	    class_name = "PPARAMREF";
	  else
	    class_name = "PAUTO";
	}
      else if (node_object->is_result_variable())
	class_name = "PPARAMOUT";
      else if (node_object->is_function()
	       || node_object->is_function_declaration())
	class_name = "PFUNC";
    }
  else if (e != NULL && e->enclosed_var_expression() != NULL)
    {
      Named_object* enclosed = e->enclosed_var_expression()->variable();
      if (enclosed->is_variable())
	{
	  Variable* var = enclosed->var_value();
	  is_address_taken = (var->is_address_taken()
			      || var->is_non_escaping_address_taken());
	}
      else
	{
	  Result_variable* var = enclosed->result_var_value();
	  is_address_taken = (var->is_address_taken()
			      || var->is_non_escaping_address_taken());
	}
      class_name = "PPARAMREF";
    }

  if (!class_name.empty())
    {
      details << " class(" << class_name;
      if (is_in_heap)
	details << ",heap";
      details << ")";
    }

  switch ((this->encoding() & ESCAPE_MASK))
    {
    case Node::ESCAPE_UNKNOWN:
      break;

    case Node::ESCAPE_HEAP:
      details << " esc(h)";
      break;

    case Node::ESCAPE_NONE:
      details << " esc(no)";
      break;

    case Node::ESCAPE_NEVER:
      details << " esc(N)";
      break;

    default:
      details << " esc(" << this->encoding() << ")";
      break;
    }

  if (this->state_ != NULL && this->state_->loop_depth != 0)
    details << " ld(" << this->state_->loop_depth << ")";

  if (is_varargs)
    details << " isddd(1)";
  if (is_address_taken)
    details << " addrtaken";
  if (is_assigned)
    details << " assigned";

  return details.str();
}

std::string
Node::op_format() const
{
  std::stringstream op;
  Ast_dump_context adc(&op, false);
  if (this->expr() != NULL)
    {
      Expression* e = this->expr();
      switch (e->classification())
	{
	case Expression::EXPRESSION_UNARY:
	  adc.dump_operator(e->unary_expression()->op());
	  break;

	case Expression::EXPRESSION_BINARY:
	  adc.dump_operator(e->binary_expression()->op());
	  break;

	case Expression::EXPRESSION_CALL:
	  op << "function call";
	  break;

	case Expression::EXPRESSION_FUNC_REFERENCE:
	  if (e->func_expression()->is_runtime_function())
	    {
	      switch (e->func_expression()->runtime_code())
		{
		case Runtime::GOPANIC:
		  op << "panic";
		  break;

		case Runtime::GROWSLICE:
		  op << "append";
		  break;

		case Runtime::SLICECOPY:
		case Runtime::SLICESTRINGCOPY:
		case Runtime::TYPEDSLICECOPY:
		  op << "copy";
		  break;

		case Runtime::MAKECHAN:
		case Runtime::MAKECHAN64:
		case Runtime::MAKEMAP:
		case Runtime::MAKESLICE:
		case Runtime::MAKESLICE64:
		  op << "make";
		  break;

		case Runtime::DEFERPROC:
		  op << "defer";
		  break;

		case Runtime::GORECOVER:
		  op << "recover";
		  break;

		case Runtime::CLOSE:
		  op << "close";
		  break;

		default:
		  break;
		}
	    }
	  break;

	case Expression::EXPRESSION_ALLOCATION:
	  op << "new";
	  break;

	case Expression::EXPRESSION_RECEIVE:
	  op << "<-";
	  break;

	default:
	  break;
	}
    }

  if (this->statement() != NULL)
    {
      switch (this->statement()->classification())
	{
	case Statement::STATEMENT_DEFER:
	  op << "defer";
	  break;

	case Statement::STATEMENT_RETURN:
	  op << "return";
	  break;

	default:
	  break;
	}
    }
  if (this->is_indirect())
    op << "*";
  return op.str();
}

// Return this node's state, creating it if has not been initialized.

Node::Escape_state*
Node::state(Escape_context* context, Named_object* fn)
{
  if (this->state_ == NULL)
    {
      if (this->expr() != NULL && this->expr()->var_expression() != NULL)
	{
	  // Tie state of variable references to underlying variables.
	  Named_object* var_no = this->expr()->var_expression()->named_object();
	  Node* var_node = Node::make_node(var_no);
	  this->state_ = var_node->state(context, fn);
	}
      else
	{
	  this->state_ = new Node::Escape_state;
	  if (fn == NULL)
	    fn = context->current_function();

	  this->state_->fn = fn;
	}
    }
  go_assert(this->state_ != NULL);
  return this->state_;
}

Node::~Node()
{
  if (this->state_ != NULL)
    {
      if (this->expr() == NULL || this->expr()->var_expression() == NULL)
        // Var expression Node is excluded since it shares state with the
        // underlying var Node.
        delete this->state_;
    }
}

int
Node::encoding()
{
  if (this->expr() != NULL
      && this->expr()->var_expression() != NULL)
    {
      // Get the underlying object's encoding.
      Named_object* no = this->expr()->var_expression()->named_object();
      int enc = Node::make_node(no)->encoding();
      this->encoding_ = enc;
    }
  return this->encoding_;
}

void
Node::set_encoding(int enc)
{
  this->encoding_ = enc;
  if (this->expr() != NULL)
    {
      if (this->expr()->var_expression() != NULL)
        {
          // Set underlying object as well.
          Named_object* no = this->expr()->var_expression()->named_object();
          Node::make_node(no)->set_encoding(enc);
        }
      else if (this->expr()->func_expression() != NULL)
        {
          // Propagate the escape state to the underlying
          // closure (heap expression).
          Expression* closure = this->expr()->func_expression()->closure();
          if (closure != NULL)
            Node::make_node(closure)->set_encoding(enc);
        }
    }
}

bool
Node::is_big(Escape_context* context) const
{
  Type* t = this->type();
  if (t == NULL
      || t->is_call_multiple_result_type()
      || t->is_sink_type()
      || t->is_void_type()
      || t->is_abstract())
    return false;

  int64_t size;
  bool ok = t->backend_type_size(context->gogo(), &size);
  bool big = ok && (size < 0 || size > 10 * 1024 * 1024);

  if (this->expr() != NULL)
    {
      if (this->expr()->allocation_expression() != NULL)
	{
	  ok = t->deref()->backend_type_size(context->gogo(), &size);
	  big = big || size <= 0 || size >= (1 << 16);
	}
      else if (this->expr()->call_expression() != NULL)
	{
	  Call_expression* call = this->expr()->call_expression();
	  Func_expression* fn = call->fn()->func_expression();
	  if (fn != NULL
	      && fn->is_runtime_function()
	      && (fn->runtime_code() == Runtime::MAKESLICE
		  || fn->runtime_code() == Runtime::MAKESLICE64))
	    {
	      // Second argument is length.
	      Expression_list::iterator p = call->args()->begin();
	      ++p;

              Expression* e = *p;
              if (e->temporary_reference_expression() != NULL)
                {
                  Temporary_reference_expression* tre = e->temporary_reference_expression();
                  if (tre->statement() != NULL && tre->statement()->init() != NULL)
                    e = tre->statement()->init();
                }

	      Numeric_constant nc;
	      unsigned long v;
	      if (e->numeric_constant_value(&nc)
		  && nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_VALID)
		big = big || v >= (1 << 16);
	    }
	}
    }

  return big;
}

bool
Node::is_sink() const
{
  if (this->object() != NULL
      && this->object()->is_sink())
    return true;
  else if (this->expr() != NULL
	   && this->expr()->is_sink_expression())
    return true;
  return false;
}

std::map<Named_object*, Node*> Node::objects;
std::map<Expression*, Node*> Node::expressions;
std::map<Statement*, Node*> Node::statements;
std::vector<Node*> Node::indirects;

// Make a object node or return a cached node for this object.

Node*
Node::make_node(Named_object* no)
{
  if (Node::objects.find(no) != Node::objects.end())
    return Node::objects[no];

  Node* n = new Node(no);
  std::pair<Named_object*, Node*> val(no, n);
  Node::objects.insert(val);
  return n;
}

// Make an expression node or return a cached node for this expression.

Node*
Node::make_node(Expression* e)
{
  if (Node::expressions.find(e) != Node::expressions.end())
    return Node::expressions[e];

  Node* n = new Node(e);
  std::pair<Expression*, Node*> val(e, n);
  Node::expressions.insert(val);
  return n;
}

// Make a statement node or return a cached node for this statement.

Node*
Node::make_node(Statement* s)
{
  if (Node::statements.find(s) != Node::statements.end())
    return Node::statements[s];

  Node* n = new Node(s);
  std::pair<Statement*, Node*> val(s, n);
  Node::statements.insert(val);
  return n;
}

// Make an indirect node with given child.

Node*
Node::make_indirect_node(Node* child)
{
  Node* n = new Node(child);
  Node::indirects.push_back(n);
  return n;
}

// Returns the maximum of an exisiting escape value
// (and its additional parameter flow flags) and a new escape type.

int
Node::max_encoding(int e, int etype)
{
  if ((e & ESCAPE_MASK) > etype)
    return e;
  if (etype == Node::ESCAPE_NONE || etype == Node::ESCAPE_RETURN)
    return (e & ~ESCAPE_MASK) | etype;
  return etype;
}

// Return a modified encoding for an input parameter that flows into an
// output parameter.

int
Node::note_inout_flows(int e, int index, Level level)
{
  // Flow+level is encoded in two bits.
  // 00 = not flow, xx = level+1 for 0 <= level <= maxEncodedLevel.
  // 16 bits for Esc allows 6x2bits or 4x3bits or 3x4bits if additional
  // information would be useful.
  if (level.value() <= 0 && level.suffix_value() > 0)
    return Node::max_encoding(e|ESCAPE_CONTENT_ESCAPES, Node::ESCAPE_NONE);
  if (level.value() < 0)
    return Node::ESCAPE_HEAP;
  if (level.value() >  ESCAPE_MAX_ENCODED_LEVEL)
    level = Level::From(ESCAPE_MAX_ENCODED_LEVEL);

  int encoded = level.value() + 1;
  int shift = ESCAPE_BITS_PER_OUTPUT_IN_TAG * index + ESCAPE_RETURN_BITS;
  int old = (e >> shift) & ESCAPE_BITS_MASK_FOR_TAG;
  if (old == 0
      || (encoded != 0 && encoded < old))
    old = encoded;

  int encoded_flow = old << shift;
  if (((encoded_flow >> shift) & ESCAPE_BITS_MASK_FOR_TAG) != old)
    {
      // Failed to encode.  Put this on the heap.
      return Node::ESCAPE_HEAP;
    }

  return (e & ~(ESCAPE_BITS_MASK_FOR_TAG << shift)) | encoded_flow;
}

// Class Escape_context.

Escape_context::Escape_context(Gogo* gogo, bool recursive)
  : gogo_(gogo), current_function_(NULL), recursive_(recursive),
    sink_(Node::make_node(Named_object::make_sink())), loop_depth_(0),
    flood_id_(0), pdepth_(0)
{
  // The sink always escapes to heap and strictly lives outside of the
  // current function i.e. loop_depth == -1.
  Node::Escape_state* state = this->sink_->state(this, NULL);
  state->loop_depth = -1;
}

std::string
debug_function_name(Named_object* fn)
{
  if (fn == NULL)
    return "<S>";

  if (!fn->is_function())
    return Gogo::unpack_hidden_name(fn->name());

  std::string fnname = Gogo::unpack_hidden_name(fn->name());
  if (fn->func_value()->is_method())
    {
      // Methods in gc compiler are named "T.m" or "(*T).m" where
      // T is the receiver type. Add the receiver here.
      Type* rt = fn->func_value()->type()->receiver()->type();
      switch (rt->classification())
	{
	case Type::TYPE_NAMED:
	  fnname = rt->named_type()->name() + "." + fnname;
	  break;

	case Type::TYPE_POINTER:
	  {
	    Named_type* nt = rt->points_to()->named_type();
	    if (nt != NULL)
	      fnname = "(*" + nt->name() + ")." + fnname;
	    break;
	  }

	default:
	  break;
	}
    }

  return fnname;
}

// Return the name of the current function.

std::string
Escape_context::current_function_name() const
{
  return debug_function_name(this->current_function_);
}

// Initialize the dummy return values for this Node N using the results
// in FNTYPE.

void
Escape_context::init_retvals(Node* n, Function_type* fntype)
{
  if (fntype == NULL || fntype->results() == NULL)
    return;

  Node::Escape_state* state = n->state(this, NULL);
  state->retvals.clear();
  Location loc = n->location();

  int i = 0;
  char buf[50];
  for (Typed_identifier_list::const_iterator p = fntype->results()->begin();
       p != fntype->results()->end();
       ++p, ++i)
    {
      snprintf(buf, sizeof buf, ".out%d", i);
      Variable* dummy_var = new Variable(p->type(), NULL, false, false,
					 false, loc);
      dummy_var->set_is_used();
      Named_object* dummy_no =
	Named_object::make_variable(buf, NULL, dummy_var);
      Node* dummy_node = Node::make_node(dummy_no);
      // Initialize the state of the dummy output node.
      Node::Escape_state* dummy_node_state = dummy_node->state(this, NULL);
      dummy_node_state->loop_depth = this->loop_depth_;

      // Add dummy node to the retvals of n.
      state->retvals.push_back(dummy_node);
    }
}


// Apply an indirection to N and return the result.

Node*
Escape_context::add_dereference(Node* n)
{
  Expression* e = n->expr();
  Location loc = n->location();
  Node* ind;
  if (e != NULL
      && e->type()->points_to() != NULL
      && !e->type()->points_to()->is_void_type())
    {
      // We don't dereference void*, which can be actually any pointer type.
      Expression* deref_expr = Expression::make_unary(OPERATOR_MULT, e, loc);
      ind = Node::make_node(deref_expr);
    }
  else
    // The gc compiler simply makes an OIND node. We can't do it
    // for non-pointer type because that will result in a type error.
    // Instead, we model this by making a node with a special flavor.
    ind = Node::make_indirect_node(n);

  // Initialize the state.
  Node::Escape_state* state = ind->state(this, NULL);
  state->loop_depth = n->state(this, NULL)->loop_depth;
  return ind;
}

void
Escape_context::track(Node* n)
{
  n->set_encoding(Node::ESCAPE_NONE);
  // Initialize this node's state if it hasn't been encountered
  // before.
  Node::Escape_state* state = n->state(this, NULL);
  state->loop_depth = this->loop_depth_;

  this->noesc_.push_back(n);
}

// Return the string representation of an escapement encoding.

std::string
Escape_note::make_tag(int encoding)
{
  char buf[50];
  snprintf(buf, sizeof buf, "esc:0x%x", encoding);
  return buf;
}

// Return the escapement encoding for a string tag.

int
Escape_note::parse_tag(std::string* tag)
{
  if (tag == NULL || tag->substr(0, 4) != "esc:")
    return Node::ESCAPE_UNKNOWN;
  int encoding = (int)strtol(tag->substr(4).c_str(), NULL, 0);
  if (encoding == 0)
    return Node::ESCAPE_UNKNOWN;
  return encoding;
}


// The -fgo-optimize-alloc flag activates this escape analysis.

Go_optimize optimize_allocation_flag("allocs", true);

// A helper function to compute whether a function name has a
// matching hash value.

static bool
escape_hash_match(std::string suffix, std::string name)
{
  if (suffix.empty())
    return true;
  if (suffix.at(0) == '-')
    return !escape_hash_match(suffix.substr(1), name);

  const char* p = name.c_str();
  Go_sha1_helper* sha1_helper = go_create_sha1_helper();
  sha1_helper->process_bytes(p, strlen(p));
  std::string s = sha1_helper->finish();
  delete sha1_helper;

  int j = suffix.size() - 1;
  for (int i = s.size() - 1; i >= 0; i--)
    {
      char c = s.at(i);
      for (int k = 0; k < 8; k++, j--, c>>=1)
        {
          if (j < 0)
            return true;
          char bit = suffix.at(j) - '0';
          if ((c&1) != bit)
            return false;
        }
    }
  return false;
}

// Analyze the program flow for escape information.

void
Gogo::analyze_escape()
{
  if (saw_errors())
    return;

  if (!optimize_allocation_flag.is_enabled()
      && !this->compiling_runtime())
    // We always run escape analysis when compiling runtime.
    return;

  // Discover strongly connected groups of functions to analyze for escape
  // information in this package.
  this->discover_analysis_sets();

  if (!this->debug_escape_hash().empty())
    std::cerr << "debug-escape-hash " << this->debug_escape_hash() << "\n";

  for (std::vector<Analysis_set>::iterator p = this->analysis_sets_.begin();
       p != this->analysis_sets_.end();
       ++p)
    {
      std::vector<Named_object*> stack = p->first;

      if (!this->debug_escape_hash().empty())
        {
          bool match = false;
          for (std::vector<Named_object*>::const_iterator fn = stack.begin();
               fn != stack.end();
               ++fn)
            match = match || escape_hash_match(this->debug_escape_hash(), (*fn)->message_name());
          if (!match)
            {
              // Escape analysis won't run on these functions, but still
              // need to tag them, so the caller knows.
              for (std::vector<Named_object*>::iterator fn = stack.begin();
                   fn != stack.end();
                   ++fn)
                if ((*fn)->is_function())
                  {
                    Function_type* fntype = (*fn)->func_value()->type();
                    fntype->set_is_tagged();

                    std::cerr << "debug-escape-hash disables " << debug_function_name(*fn) << "\n";
                  }

              continue;
            }
          for (std::vector<Named_object*>::const_iterator fn = stack.begin();
               fn != stack.end();
               ++fn)
            if ((*fn)->is_function())
              std::cerr << "debug-escape-hash triggers " << debug_function_name(*fn) << "\n";
        }

      Escape_context* context = new Escape_context(this, p->second);

      // Analyze the flow of each function; build the connection graph.
      // This is the assign phase.
      for (std::vector<Named_object*>::reverse_iterator fn = stack.rbegin();
           fn != stack.rend();
           ++fn)
	{
	  context->set_current_function(*fn);
	  this->assign_connectivity(context, *fn);
	}

      // Propagate levels across each dst.  This is the flood phase.
      std::set<Node*> dsts = context->dsts();
      Unordered_map(Node*, int) escapes;
      for (std::set<Node*>::iterator n = dsts.begin();
           n != dsts.end();
           ++n)
        {
          escapes[*n] = (*n)->encoding();
          this->propagate_escape(context, *n);
        }
      for (;;)
        {
          // Reflood if the roots' escape states increase. Run until fix point.
          // This is rare.
          bool done = true;
          for (std::set<Node*>::iterator n = dsts.begin();
               n != dsts.end();
               ++n)
            {
              if ((*n)->object() == NULL
                  && ((*n)->expr() == NULL
                      || ((*n)->expr()->var_expression() == NULL
                          && (*n)->expr()->enclosed_var_expression() == NULL
                          && (*n)->expr()->func_expression() == NULL)))
                continue;
              if (escapes[*n] != (*n)->encoding())
                {
                  done = false;
                  if (this->debug_escape_level() > 2)
                    go_inform((*n)->location(), "Reflooding %s %s",
                              debug_function_name((*n)->state(context, NULL)->fn).c_str(),
                              (*n)->ast_format(this).c_str());
                  escapes[*n] = (*n)->encoding();
                  this->propagate_escape(context, *n);
                }
            }
          if (done)
            break;
        }

      // Tag each exported function's parameters with escape information.
      for (std::vector<Named_object*>::iterator fn = stack.begin();
           fn != stack.end();
           ++fn)
        this->tag_function(context, *fn);

      if (this->debug_escape_level() != 0)
	{
	  std::vector<Node*> noesc = context->non_escaping_nodes();
	  for (std::vector<Node*>::const_iterator n = noesc.begin();
	       n != noesc.end();
	       ++n)
	    {
	      Node::Escape_state* state = (*n)->state(context, NULL);
	      if ((*n)->encoding() == Node::ESCAPE_NONE)
		go_inform((*n)->location(), "%s %s does not escape",
			  strip_packed_prefix(this, debug_function_name(state->fn)).c_str(),
			  (*n)->ast_format(this).c_str());
	    }
	}
      delete context;
    }
}

// Traverse the program, discovering the functions that are roots of strongly
// connected components.  The goal of this phase to produce a set of functions
// that must be analyzed in order.

class Escape_analysis_discover : public Traverse
{
 public:
  Escape_analysis_discover(Gogo* gogo)
    : Traverse(traverse_functions | traverse_func_declarations),
      gogo_(gogo), component_ids_()
  { }

  int
  function(Named_object*);

  int
  function_declaration(Named_object*);

  int
  visit(Named_object*);

  int
  visit_code(Named_object*, int);

 private:
  // A counter used to generate the ID for the function node in the graph.
  static int id;

  // Type used to map functions to an ID in a graph of connected components.
  typedef Unordered_map(Named_object*, int) Component_ids;

  // The Go IR.
  Gogo* gogo_;
  // The list of functions encountered during connected component discovery.
  Component_ids component_ids_;
  // The stack of functions that this component consists of.
  std::stack<Named_object*> stack_;
};

int Escape_analysis_discover::id = 0;

// Visit each function.

int
Escape_analysis_discover::function(Named_object* fn)
{
  this->visit(fn);
  return TRAVERSE_CONTINUE;
}

int
Escape_analysis_discover::function_declaration(Named_object* fn)
{
  this->visit(fn);
  return TRAVERSE_CONTINUE;
}

// Visit a function FN, adding it to the current stack of functions
// in this connected component.  If this is the root of the component,
// create a set of functions to be analyzed later.
//
// Finding these sets is finding strongly connected components
// in the static call graph.  The algorithm for doing that is taken
// from Sedgewick, Algorithms, Second Edition, p. 482, with two
// adaptations.
//
// First, a closure (fn->func_value()->enclosing() == NULL) cannot be the
// root of a connected component.  Refusing to use it as a root
// forces it into the component of the function in which it appears.
// This is more convenient for escape analysis.
//
// Second, each function becomes two virtual nodes in the graph,
// with numbers n and n+1. We record the function's node number as n
// but search from node n+1. If the search tells us that the component
// number (min) is n+1, we know that this is a trivial component: one function
// plus its closures. If the search tells us that the component number is
// n, then there was a path from node n+1 back to node n, meaning that
// the function set is mutually recursive. The escape analysis can be
// more precise when analyzing a single non-recursive function than
// when analyzing a set of mutually recursive functions.

int
Escape_analysis_discover::visit(Named_object* fn)
{
  Component_ids::const_iterator p = this->component_ids_.find(fn);
  if (p != this->component_ids_.end())
    // Already visited.
    return p->second;

  this->id++;
  int id = this->id;
  this->component_ids_[fn] = id;
  this->id++;
  int min = this->id;

  this->stack_.push(fn);
  min = this->visit_code(fn, min);
  if ((min == id || min == id + 1)
      && ((fn->is_function() && fn->func_value()->enclosing() == NULL)
          || fn->is_function_declaration()))
    {
      bool recursive = min == id;
      std::vector<Named_object*> group;

      for (; !this->stack_.empty(); this->stack_.pop())
	{
	  Named_object* n = this->stack_.top();
	  if (n == fn)
	    {
	      this->stack_.pop();
	      break;
	    }

	  group.push_back(n);
	  this->component_ids_[n] = std::numeric_limits<int>::max();
	}
      group.push_back(fn);
      this->component_ids_[fn] = std::numeric_limits<int>::max();

      std::reverse(group.begin(), group.end());
      this->gogo_->add_analysis_set(group, recursive);
    }

  return min;
}

// Helper class for discovery step.  Traverse expressions looking for
// function calls and closures to visit during the discovery step.

class Escape_discover_expr : public Traverse
{
 public:
  Escape_discover_expr(Escape_analysis_discover* ead, int min)
    : Traverse(traverse_expressions),
      ead_(ead), min_(min)
  { }

  int
  min()
  { return this->min_; }

  int
  expression(Expression** pexpr);

 private:
  // The original discovery analysis.
  Escape_analysis_discover* ead_;
  // The minimum component ID in this group.
  int min_;
};

// Visit any calls or closures found when discovering expressions.

int
Escape_discover_expr::expression(Expression** pexpr)
{
  Expression* e = *pexpr;
  Named_object* fn = NULL;
  if (e->call_expression() != NULL
      && e->call_expression()->fn()->func_expression() != NULL)
    {
      // Method call or function call.
      fn = e->call_expression()->fn()->func_expression()->named_object();
    }
  else if (e->func_expression() != NULL
           && e->func_expression()->closure() != NULL)
    {
      // Closure.
      fn = e->func_expression()->named_object();
    }

  if (fn != NULL)
    this->min_ = std::min(this->min_, this->ead_->visit(fn));
  return TRAVERSE_CONTINUE;
}

// Visit the body of each function, returns ID of the minimum connected
// component found in the body.

int
Escape_analysis_discover::visit_code(Named_object* fn, int min)
{
  if (!fn->is_function())
    return min;

  Escape_discover_expr ede(this, min);
  fn->func_value()->traverse(&ede);
  return ede.min();
}

// Discover strongly connected groups of functions to analyze.

void
Gogo::discover_analysis_sets()
{
  Escape_analysis_discover ead(this);
  this->traverse(&ead);
}

// Traverse all label and goto statements and mark the underlying label
// as looping or not looping.

class Escape_analysis_loop : public Traverse
{
 public:
  Escape_analysis_loop()
    : Traverse(traverse_statements)
  { }

  int
  statement(Block*, size_t*, Statement*);
};

int
Escape_analysis_loop::statement(Block*, size_t*, Statement* s)
{
  if (s->label_statement() != NULL)
    s->label_statement()->label()->set_nonlooping();
  else if (s->goto_statement() != NULL)
    {
      if (s->goto_statement()->label()->nonlooping())
        s->goto_statement()->label()->set_looping();
    }
  return TRAVERSE_CONTINUE;
}

// Traversal class used to look at all interesting statements within a function
// in order to build a connectivity graph between all nodes within a context's
// scope.

class Escape_analysis_assign : public Traverse
{
public:
  Escape_analysis_assign(Escape_context* context, Named_object* fn)
    : Traverse(traverse_statements
	       | traverse_expressions),
      context_(context), fn_(fn)
  { }

  // Model statements within a function as assignments and flows between nodes.
  int
  statement(Block*, size_t*, Statement*);

  // Model expressions within a function as assignments and flows between nodes.
  int
  expression(Expression**);

  // Model calls within a function as assignments and flows between arguments
  // and results.
  void
  call(Call_expression* call);

  // Model the assignment of DST to SRC.
  void
  assign(Node* dst, Node* src);

  // Model the assignment of DST to dereference of SRC.
  void
  assign_deref(Node* dst, Node* src);

  // Model the input-to-output assignment flow of one of a function call's
  // arguments, where the flow is encoding in NOTE.
  int
  assign_from_note(std::string* note, const std::vector<Node*>& dsts,
		   Node* src);

  // Record the flow of SRC to DST in DST.
  void
  flows(Node* dst, Node* src);

private:
  // The escape context for this set of functions.
  Escape_context* context_;
  // The current function being analyzed.
  Named_object* fn_;
};

// Helper function to detect self assignment like the following.
//
// func (b *Buffer) Foo() {
//   n, m := ...
//   b.buf = b.buf[n:m]
// }

static bool
is_self_assignment(Expression* lhs, Expression* rhs)
{
  Unary_expression* lue =
    (lhs->field_reference_expression() != NULL
     ? lhs->field_reference_expression()->expr()->unary_expression()
     : lhs->unary_expression());
  Var_expression* lve =
    (lue != NULL && lue->op() == OPERATOR_MULT ? lue->operand()->var_expression() : NULL);
  Array_index_expression* raie = rhs->array_index_expression();
  String_index_expression* rsie = rhs->string_index_expression();
  Expression* rarray =
    (raie != NULL && raie->end() != NULL && raie->array()->type()->is_slice_type()
     ? raie->array()
     : (rsie != NULL && rsie->type()->is_string_type() ? rsie->string() : NULL));
  Unary_expression* rue =
    (rarray != NULL && rarray->field_reference_expression() != NULL
     ? rarray->field_reference_expression()->expr()->unary_expression()
     : (rarray != NULL ? rarray->unary_expression() : NULL));
  Var_expression* rve =
    (rue != NULL && rue->op() == OPERATOR_MULT ? rue->operand()->var_expression() : NULL);
  return lve != NULL && rve != NULL
         && lve->named_object() == rve->named_object();
}

// Model statements within a function as assignments and flows between nodes.

int
Escape_analysis_assign::statement(Block*, size_t*, Statement* s)
{
  // Adjust the loop depth as we enter/exit blocks related to for statements.
  bool is_for_statement = (s->is_block_statement()
                           && s->block_statement()->is_lowered_for_statement());
  if (is_for_statement)
    this->context_->increase_loop_depth();

  s->traverse_contents(this);

  if (is_for_statement)
    this->context_->decrease_loop_depth();

  Gogo* gogo = this->context_->gogo();
  int debug_level = gogo->debug_escape_level();
  if (debug_level > 1
      && s->unnamed_label_statement() == NULL
      && s->expression_statement() == NULL
      && !s->is_block_statement())
    {
      Node* n = Node::make_node(s);
      std::string fn_name = this->context_->current_function_name();
      go_inform(s->location(), "[%d] %s esc: %s",
	        this->context_->loop_depth(), fn_name.c_str(),
	        n->ast_format(gogo).c_str());
    }

  switch (s->classification())
    {
    case Statement::STATEMENT_VARIABLE_DECLARATION:
      {
	Named_object* var = s->variable_declaration_statement()->var();
	Node* var_node = Node::make_node(var);
	Node::Escape_state* state = var_node->state(this->context_, NULL);
	state->loop_depth = this->context_->loop_depth();

	// Set the loop depth for this declaration.
	if (var->is_variable()
	    && var->var_value()->init() != NULL)
	  {
	    Node* init_node = Node::make_node(var->var_value()->init());
	    this->assign(var_node, init_node);
	  }
      }
      break;

    case Statement::STATEMENT_TEMPORARY:
      {
        Expression* init = s->temporary_statement()->init();
        if (init != NULL)
          this->assign(Node::make_node(s), Node::make_node(init));
      }
      break;

    case Statement::STATEMENT_LABEL:
      {
	Label_statement* label_stmt = s->label_statement();
	if (label_stmt->label()->looping())
	  this->context_->increase_loop_depth();

	if (debug_level > 1)
	  {
	    std::string label_type = (label_stmt->label()->looping()
				      ? "looping"
				      : "nonlooping");
	    go_inform(s->location(), "%s %s label",
		      label_stmt->label()->name().c_str(),
		      label_type.c_str());
	  }
      }
      break;

    case Statement::STATEMENT_SWITCH:
    case Statement::STATEMENT_TYPE_SWITCH:
      // Want to model the assignment of each case variable to the switched upon
      // variable.  This should be lowered into assignment statements; nothing
      // to here if that's the case.
      break;

    case Statement::STATEMENT_ASSIGNMENT:
      {
	Assignment_statement* assn = s->assignment_statement();
        Expression* lhs = assn->lhs();
        Expression* rhs = assn->rhs();
        Node* lhs_node = Node::make_node(lhs);
        Node* rhs_node = Node::make_node(rhs);

        // Filter out the following special case.
        //
        // func (b *Buffer) Foo() {
        //   n, m := ...
        //   b.buf = b.buf[n:m]
        // }
        //
        // This assignment is a no-op for escape analysis,
        // it does not store any new pointers into b that were not already there.
        // However, without this special case b will escape.
        if (is_self_assignment(lhs, rhs))
          {
            if (debug_level != 0)
              go_inform(s->location(), "%s ignoring self-assignment to %s",
                        strip_packed_prefix(gogo, this->context_->current_function_name()).c_str(),
                        lhs_node->ast_format(gogo).c_str());
            break;
          }

        this->assign(lhs_node, rhs_node);
      }
      break;

    case Statement::STATEMENT_SEND:
      {
	Node* sent_node = Node::make_node(s->send_statement()->val());
	this->assign(this->context_->sink(), sent_node);
      }
      break;

    case Statement::STATEMENT_DEFER:
      if (this->context_->loop_depth() == 1)
        {
          // Defer statement may need to allocate a thunk. When it is
          // not inside a loop, this can be stack allocated, as it
          // runs before the function finishes.
          Node* n = Node::make_node(s);
          n->set_encoding(Node::ESCAPE_NONE);
          break;
        }
      // fallthrough

    case Statement::STATEMENT_GO:
      {
	// Defer f(x) or go f(x).
	// Both f and x escape to the heap.
	Thunk_statement* thunk = s->thunk_statement();
	if (thunk->call()->call_expression() == NULL)
	  break;

	Call_expression* call = thunk->call()->call_expression();
	Node* func_node = Node::make_node(call->fn());
	this->assign(this->context_->sink(), func_node);
	if (call->args() != NULL)
	  {
	    for (Expression_list::const_iterator p = call->args()->begin();
		 p != call->args()->end();
		 ++p)
	      {
		Node* arg_node = Node::make_node(*p);
		this->assign(this->context_->sink(), arg_node);
	      }
	  }
      }
      break;

    default:
      break;
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Helper function to emit moved-to-heap diagnostics.

static void
move_to_heap(Gogo* gogo, Expression *expr)
{
  Named_object* no;
  if (expr->var_expression() != NULL)
    no = expr->var_expression()->named_object();
  else if (expr->enclosed_var_expression() != NULL)
    no = expr->enclosed_var_expression()->variable();
  else
    return;

  if ((no->is_variable()
       && !no->var_value()->is_global())
      || no->is_result_variable())
    {
      Node* n = Node::make_node(expr);
      if (gogo->debug_escape_level() != 0)
        go_inform(n->definition_location(),
                  "moved to heap: %s",
                  n->ast_format(gogo).c_str());
      if (gogo->compiling_runtime() && gogo->package_name() == "runtime")
        go_error_at(expr->location(),
                    "%s escapes to heap, not allowed in runtime",
                    n->ast_format(gogo).c_str());
    }
}

// Model expressions within a function as assignments and flows between nodes.

int
Escape_analysis_assign::expression(Expression** pexpr)
{
  Gogo* gogo = this->context_->gogo();
  int debug_level = gogo->debug_escape_level();

  // Big stuff escapes unconditionally.
  Node* n = Node::make_node(*pexpr);
  if ((n->encoding() & ESCAPE_MASK) != int(Node::ESCAPE_HEAP)
      && n->is_big(this->context_))
    {
      if (debug_level > 1)
	go_inform((*pexpr)->location(), "%s too large for stack",
                  n->ast_format(gogo).c_str());
      move_to_heap(gogo, *pexpr);
      n->set_encoding(Node::ESCAPE_HEAP);
      (*pexpr)->address_taken(true);
      this->assign(this->context_->sink(), n);
    }

  if ((*pexpr)->func_expression() == NULL)
    (*pexpr)->traverse_subexpressions(this);

  if (debug_level > 1)
    {
      Node* n = Node::make_node(*pexpr);
      std::string fn_name = this->context_->current_function_name();
      go_inform((*pexpr)->location(), "[%d] %s esc: %s",
		this->context_->loop_depth(), fn_name.c_str(),
		n->ast_format(gogo).c_str());
    }

  switch ((*pexpr)->classification())
    {
    case Expression::EXPRESSION_CALL:
      {
	Call_expression* call = (*pexpr)->call_expression();
        if (call->is_builtin())
          {
            Builtin_call_expression* bce = call->builtin_call_expression();
            switch (bce->code())
              {
              case Builtin_call_expression::BUILTIN_PANIC:
                {
                  // Argument could leak through recover.
                  Node* panic_arg = Node::make_node(call->args()->front());
                  this->assign(this->context_->sink(), panic_arg);
                }
                break;

              case Builtin_call_expression::BUILTIN_APPEND:
                {
                  // The contents being appended leak.
                  if (call->is_varargs())
                    {
                      // append(slice1, slice2...) -- slice2 itself does not escape, but contents do
                      Node* appended = Node::make_node(call->args()->back());
                      this->assign_deref(this->context_->sink(), appended);
                      if (debug_level > 2)
                        go_inform((*pexpr)->location(),
                                  "special treatment of append(slice1, slice2...)");
                    }
                  else
                    {
                      for (Expression_list::const_iterator pa =
                             call->args()->begin() + 1;
                           pa != call->args()->end();
                           ++pa)
                        {
                          Node* arg = Node::make_node(*pa);
                          this->assign(this->context_->sink(), arg);
                        }
                    }

                  // The content of the original slice leaks as well.
                  Node* appendee = Node::make_node(call->args()->front());
                  this->assign_deref(this->context_->sink(), appendee);
                }
                break;

              case Builtin_call_expression::BUILTIN_COPY:
                {
                  // Lose track of the copied content.
                  Node* copied = Node::make_node(call->args()->back());
                  this->assign_deref(this->context_->sink(), copied);
                }
                break;

              default:
                break;
              }
            break;
          }
	Func_expression* fe = call->fn()->func_expression();
	if (fe != NULL && fe->is_runtime_function())
	  {
	    switch (fe->runtime_code())
	      {
	      case Runtime::MAKECHAN:
	      case Runtime::MAKECHAN64:
	      case Runtime::MAKEMAP:
	      case Runtime::MAKESLICE:
	      case Runtime::MAKESLICE64:
                this->context_->track(n);
		break;

              case Runtime::MAPASSIGN:
                {
                  // Map key escapes. The last argument is the address
                  // of the key.
                  Node* key_node = Node::make_node(call->args()->back());
                  this->assign_deref(this->context_->sink(), key_node);
                }
                break;

              case Runtime::SELECTSEND:
                {
                  // Send to a channel, lose track. The last argument is
                  // the address of the value to send.
                  Node* arg_node = Node::make_node(call->args()->back());
                  this->assign_deref(this->context_->sink(), arg_node);
                }
                break;

              case Runtime::IFACEE2T2:
              case Runtime::IFACEI2T2:
                {
                  // x, ok = v.(T), where T is non-pointer non-interface,
                  // is lowered to
                  // ok = IFACEI2T2(type, v, (void*)&tmp_x)
                  // Here v flows to tmp_x.
                  // Note: other IFACEX2Y2 returns the conversion result.
                  // Those are handled in ::assign.
                  Node* src_node = Node::make_node(call->args()->at(1));
                  Node* dst_node;
                  Expression* arg2 = call->args()->at(2);
                  // Try to pull tmp_x out of the arg2 expression, and let v
                  // flows into it, instead of simply dereference arg2,
                  // which looks like dereference of an arbitrary pointer
                  // and causes v immediately escape.
                  // The expression form matches statement.cc,
                  // Tuple_type_guard_assignment_statement::lower_to_object_type.
                  Unary_expression* ue =
                    (arg2->conversion_expression() != NULL
                     ? arg2->conversion_expression()->expr()->unary_expression()
                     : arg2->unary_expression());
                  if (ue != NULL && ue->op() == OPERATOR_AND)
                    {
                      if (!ue->operand()->type()->has_pointer())
                        // Don't bother flowing non-pointer.
                        break;
                      dst_node = Node::make_node(ue->operand());
                    }
                  else
                    dst_node = this->context_->add_dereference(Node::make_node(arg2));
                  this->assign(dst_node, src_node);
                }
                break;

	      default:
		break;
	      }
	  }
        else
          this->call(call);
      }
      break;

    case Expression::EXPRESSION_ALLOCATION:
      // This is Runtime::NEW.
      this->context_->track(n);
      break;

    case Expression::EXPRESSION_STRING_CONCAT:
      this->context_->track(n);
      break;

    case Expression::EXPRESSION_CONVERSION:
      {
	Type_conversion_expression* tce = (*pexpr)->conversion_expression();
        Type* ft = tce->expr()->type();
        Type* tt = tce->type();
        if ((ft->is_string_type() && tt->is_slice_type())
            || (ft->is_slice_type() && tt->is_string_type())
            || (ft->integer_type() != NULL && tt->is_string_type()))
          {
            // string([]byte), string([]rune), []byte(string), []rune(string), string(rune)
            this->context_->track(n);
            break;
          }
	Node* tce_node = Node::make_node(tce);
	Node* converted = Node::make_node(tce->expr());
	this->context_->track(tce_node);

	this->assign(tce_node, converted);
      }
      break;

    case Expression::EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
    case Expression::EXPRESSION_SLICE_CONSTRUCTION:
      {
	Node* array_node = Node::make_node(*pexpr);
	if ((*pexpr)->slice_literal() != NULL)
	  this->context_->track(array_node);

	Expression_list* vals = ((*pexpr)->slice_literal() != NULL
				 ? (*pexpr)->slice_literal()->vals()
				 : (*pexpr)->array_literal()->vals());

	if (vals != NULL)
	  {
	    // Connect the array to its values.
	    for (Expression_list::const_iterator p = vals->begin();
		 p != vals->end();
		 ++p)
	      if ((*p) != NULL)
		this->assign(array_node, Node::make_node(*p));
	  }
      }
      break;

    case Expression::EXPRESSION_STRUCT_CONSTRUCTION:
      {
	Node* struct_node = Node::make_node(*pexpr);
	Expression_list* vals = (*pexpr)->struct_literal()->vals();
	if (vals != NULL)
	  {
	    // Connect the struct to its values.
	    for (Expression_list::const_iterator p = vals->begin();
		 p != vals->end();
		 ++p)
	      {
		if ((*p) != NULL)
		  this->assign(struct_node, Node::make_node(*p));
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_HEAP:
      {
	Node* pointer_node = Node::make_node(*pexpr);
	Node* lit_node = Node::make_node((*pexpr)->heap_expression()->expr());
	this->context_->track(pointer_node);

	// Connect pointer node to literal node; if the pointer node escapes, so
	// does the literal node.
	this->assign(pointer_node, lit_node);
      }
      break;

    case Expression::EXPRESSION_BOUND_METHOD:
      {
	Node* bound_node = Node::make_node(*pexpr);
	this->context_->track(bound_node);

	Expression* obj = (*pexpr)->bound_method_expression()->first_argument();
	Node* obj_node = Node::make_node(obj);

	// A bound method implies the receiver will be used outside of the
	// lifetime of the method in some way.  We lose track of the receiver.
	this->assign(this->context_->sink(), obj_node);
      }
      break;

    case Expression::EXPRESSION_MAP_CONSTRUCTION:
      {
	Map_construction_expression* mce = (*pexpr)->map_literal();
	Node* map_node = Node::make_node(mce);
	this->context_->track(map_node);

	// All keys and values escape to memory.
	if (mce->vals() != NULL)
	  {
	    for (Expression_list::const_iterator p = mce->vals()->begin();
		 p != mce->vals()->end();
		 ++p)
	      {
		if ((*p) != NULL)
		  this->assign(this->context_->sink(), Node::make_node(*p));
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_FUNC_REFERENCE:
      {
	Func_expression* fe = (*pexpr)->func_expression();
	if (fe->closure() != NULL)
	  {
	    // Connect captured variables to the closure.
	    Node* closure_node = Node::make_node(fe);
	    this->context_->track(closure_node);

	    // A closure expression already exists as the heap expression:
	    // &struct{f func_code, v []*Variable}{...}.
	    // Link closure to the addresses of the variables enclosed.
	    Heap_expression* he = fe->closure()->heap_expression();
	    Struct_construction_expression* sce = he->expr()->struct_literal();

	    // First field is function code, other fields are variable
	    // references.
	    Expression_list::const_iterator p = sce->vals()->begin();
	    ++p;
	    for (; p != sce->vals()->end(); ++p)
	      {
		Node* enclosed_node = Node::make_node(*p);
		this->context_->track(enclosed_node);
		this->assign(closure_node, enclosed_node);
	      }
	  }
      }
      break;

    case Expression::EXPRESSION_UNARY:
      {
	Expression* operand = (*pexpr)->unary_expression()->operand();

        if ((*pexpr)->unary_expression()->op() == OPERATOR_AND)
          {
            this->context_->track(n);

            Named_object* var = NULL;
            if (operand->var_expression() != NULL)
              var = operand->var_expression()->named_object();
            else if (operand->enclosed_var_expression() != NULL)
              var = operand->enclosed_var_expression()->variable();

            if (var != NULL
                && ((var->is_variable() && var->var_value()->is_parameter())
                    || var->is_result_variable()))
              {
                Node::Escape_state* addr_state = n->state(this->context_, NULL);
                addr_state->loop_depth = 1;
                break;
              }
          }

        if ((*pexpr)->unary_expression()->op() != OPERATOR_AND
            && (*pexpr)->unary_expression()->op() != OPERATOR_MULT)
          break;

        // For &x and *x, use the loop depth of x if known.
        Node::Escape_state* expr_state = n->state(this->context_, NULL);
        Node* operand_node = Node::make_node(operand);
        Node::Escape_state* operand_state = operand_node->state(this->context_, NULL);
        if (operand_state->loop_depth != 0)
          expr_state->loop_depth = operand_state->loop_depth;
      }
      break;

    case Expression::EXPRESSION_ARRAY_INDEX:
      {
        Array_index_expression* aie = (*pexpr)->array_index_expression();

        // Propagate the loopdepth to element.
        Node* array_node = Node::make_node(aie->array());
        Node::Escape_state* elem_state = n->state(this->context_, NULL);
        Node::Escape_state* array_state = array_node->state(this->context_, NULL);
        elem_state->loop_depth = array_state->loop_depth;

        if (aie->end() != NULL && !aie->array()->type()->is_slice_type())
          {
            // Slicing an array. This effectively takes the address of the array.
            Expression* addr = Expression::make_unary(OPERATOR_AND, aie->array(),
                                                      aie->location());
            Node* addr_node = Node::make_node(addr);
            n->set_child(addr_node);
            this->context_->track(addr_node);

            Node::Escape_state* addr_state = addr_node->state(this->context_, NULL);
            addr_state->loop_depth = array_state->loop_depth;
          }
      }
      break;

    case Expression::EXPRESSION_FIELD_REFERENCE:
      {
        // Propagate the loopdepth to field.
        Node* struct_node =
          Node::make_node((*pexpr)->field_reference_expression()->expr());
        Node::Escape_state* field_state = n->state(this->context_, NULL);
        Node::Escape_state* struct_state = struct_node->state(this->context_, NULL);
        field_state->loop_depth = struct_state->loop_depth;
      }
      break;

    default:
      break;
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Model calls within a function as assignments and flows between arguments
// and results.

void
Escape_analysis_assign::call(Call_expression* call)
{
  Gogo* gogo = this->context_->gogo();
  int debug_level = gogo->debug_escape_level();

  Func_expression* fn = call->fn()->func_expression();
  Function_type* fntype = call->get_function_type();
  bool indirect = false;

  // Interface method calls or closure calls are indirect calls.
  if (fntype == NULL
      || (fntype->is_method()
	  && fntype->receiver()->type()->interface_type() != NULL)
      || fn == NULL
      || (fn->named_object()->is_function()
	  && fn->named_object()->func_value()->enclosing() != NULL))
    indirect = true;

  Node* call_node = Node::make_node(call);
  std::vector<Node*> arg_nodes;
  if (call->fn()->interface_field_reference_expression() != NULL)
    {
      Interface_field_reference_expression* ifre =
	call->fn()->interface_field_reference_expression();
      Node* field_node = Node::make_node(ifre->expr());
      arg_nodes.push_back(field_node);
    }

  if (call->args() != NULL)
    {
      for (Expression_list::const_iterator p = call->args()->begin();
	   p != call->args()->end();
	   ++p)
	arg_nodes.push_back(Node::make_node(*p));
    }

  if (indirect)
    {
      // We don't know what happens to the parameters through indirect calls.
      // Be conservative and assume they all flow to theSink.
      for (std::vector<Node*>::iterator p = arg_nodes.begin();
           p != arg_nodes.end();
           ++p)
	{
	  if (debug_level > 2)
	    go_inform(call->location(),
		      "esccall:: indirect call <- %s, untracked",
		      (*p)->ast_format(gogo).c_str());
	  this->assign(this->context_->sink(), *p);
	}

      this->context_->init_retvals(call_node, fntype);

      // It could be a closure call that returns captured variable.
      // Model this by flowing the func expression to result.
      // See issue #14409.
      Node* fn_node = Node::make_node(call->fn());
      std::vector<Node*> retvals = call_node->state(this->context_, NULL)->retvals;
      for (std::vector<Node*>::const_iterator p = retvals.begin();
           p != retvals.end();
           ++p)
        this->assign_deref(*p, fn_node);

      return;
    }

  // If FN is an untagged function.
  if (fn != NULL
      && fn->named_object()->is_function()
      && !fntype->is_tagged())
    {
      if (debug_level > 2)
	go_inform(call->location(), "esccall:: %s in recursive group",
		  call_node->ast_format(gogo).c_str());

      Function* f = fn->named_object()->func_value();
      const Bindings* callee_bindings = f->block()->bindings();
      Function::Results* results = f->result_variables();
      if (results != NULL)
	{
	  // Setup output list on this call node.
	  Node::Escape_state* state = call_node->state(this->context_, NULL);
	  for (Function::Results::const_iterator p1 = results->begin();
	       p1 != results->end();
	       ++p1)
	    {
	      Node* result_node = Node::make_node(*p1);
	      state->retvals.push_back(result_node);
	    }
	}

      std::vector<Node*>::iterator p = arg_nodes.begin();
      if (fntype->is_method())
	{
	  std::string rcvr_name = fntype->receiver()->name();
	  if (rcvr_name.empty() || Gogo::is_sink_name(rcvr_name)
              || !fntype->receiver()->type()->has_pointer())
	    ;
	  else
	    {
	      Named_object* rcvr_no =
		callee_bindings->lookup_local(fntype->receiver()->name());
	      go_assert(rcvr_no != NULL);
	      Node* rcvr_node = Node::make_node(rcvr_no);
              if (fntype->receiver()->type()->points_to() == NULL
                  && (*p)->expr()->type()->points_to() != NULL)
                // This is a call to a value method that has been lowered into a call
                // to a pointer method.  Gccgo generates a pointer method for all
                // method calls and takes the address of the value passed as the
                // receiver then immediately dereferences it within the function.
                // In this case, the receiver address does not escape; its content
                // flows to the call.
                this->assign_deref(rcvr_node, *p);
              else
                this->assign(rcvr_node, *p);
	    }
	  ++p;
	}

      const Typed_identifier_list* til = fntype->parameters();
      if (til != NULL)
	{
	  for (Typed_identifier_list::const_iterator p1 = til->begin();
	       p1 != til->end();
	       ++p1, ++p)
	    {
	      if (p1->name().empty() || Gogo::is_sink_name(p1->name()))
		continue;

	      Named_object* param_no =
		callee_bindings->lookup_local(p1->name());
	      go_assert(param_no != NULL);
	      Expression* arg = (*p)->expr();
	      if (arg->var_expression() != NULL
		  && arg->var_expression()->named_object() == param_no)
		continue;

	      Node* param_node = Node::make_node(param_no);
	      this->assign(param_node, *p);
	    }

	  for (; p != arg_nodes.end(); ++p)
	    {
	      if (debug_level > 2)
		go_inform(call->location(), "esccall:: ... <- %s, untracked",
			  (*p)->ast_format(gogo).c_str());
	      this->assign(this->context_->sink(), *p);
	    }
	}

      return;
    }

  if (debug_level > 2)
    go_inform(call->location(), "esccall:: %s not recursive",
	      call_node->ast_format(gogo).c_str());

  Node::Escape_state* call_state = call_node->state(this->context_, NULL);
  if (!call_state->retvals.empty())
    go_error_at(Linemap::unknown_location(),
		"esc already decorated call %s",
		call_node->ast_format(gogo).c_str());
  this->context_->init_retvals(call_node, fntype);

  // Receiver.
  std::vector<Node*>::iterator p = arg_nodes.begin();
  if (fntype->is_method()
      && p != arg_nodes.end())
    {
      // First argument to call will be the receiver.
      std::string* note = fntype->receiver()->note();
      if (fntype->receiver()->type()->points_to() == NULL
          && (*p)->expr()->type()->points_to() != NULL)
        // This is a call to a value method that has been lowered into a call
        // to a pointer method.  Gccgo generates a pointer method for all
        // method calls and takes the address of the value passed as the
        // receiver then immediately dereferences it within the function.
        // In this case, the receiver address does not escape; its content
        // flows to the call.
        this->assign_from_note(note, call_state->retvals,
                               this->context_->add_dereference(*p));
      else
	{
	  if (!Type::are_identical(fntype->receiver()->type(),
			       (*p)->expr()->type(), true, NULL))
	    {
	      // This will be converted later, preemptively track it instead
	      // of its conversion expression which will show up in a later pass.
	      this->context_->track(*p);
	    }
	  this->assign_from_note(note, call_state->retvals, *p);
	}
      p++;
    }

  const Typed_identifier_list* til = fntype->parameters();
  if (til != NULL)
    {
      for (Typed_identifier_list::const_iterator pn = til->begin();
	   pn != til->end() && p != arg_nodes.end();
	   ++pn, ++p)
	{
	  if (!Type::are_identical(pn->type(), (*p)->expr()->type(),
				   true, NULL))
	    {
	      // This will be converted later, preemptively track it instead
	      // of its conversion expression which will show up in a later pass.
	      this->context_->track(*p);
	    }

	  Type* t = pn->type();
	  if (t != NULL
	      && t->has_pointer())
	    {
	      std::string* note = pn->note();
	      int enc = this->assign_from_note(note, call_state->retvals, *p);
	      if (enc == Node::ESCAPE_NONE
		  && !call->is_deferred()
		  && !call->is_concurrent())
		{
                  // TODO(cmang): Mark the argument as strictly non-escaping?
                  // In the gc compiler this is for limiting the lifetime of
                  // temporaries. We probably don't need this?
		}
	    }
	}

      for (; p != arg_nodes.end(); ++p)
	{
	  if (debug_level > 2)
	    go_inform(call->location(), "esccall:: ... <- %s, untracked",
                      (*p)->ast_format(gogo).c_str());
	  this->assign(this->context_->sink(), *p);
	}
    }
}

// Model the assignment of DST to SRC.
// Assert that SRC somehow gets assigned to DST.
// DST might need to be examined for evaluations that happen inside of it.
// For example, in [DST]*f(x) = [SRC]y, we lose track of the indirection and
// DST becomes the sink in our model.

void
Escape_analysis_assign::assign(Node* dst, Node* src)
{
  Gogo* gogo = this->context_->gogo();
  int debug_level = gogo->debug_escape_level();
  if (debug_level > 1)
    go_inform(dst->location(), "[%d] %s escassign: %s(%s)[%s] = %s(%s)[%s]",
	      this->context_->loop_depth(),
	      strip_packed_prefix(gogo, this->context_->current_function_name()).c_str(),
	      dst->ast_format(gogo).c_str(), dst->details().c_str(),
	      dst->op_format().c_str(),
	      src->ast_format(gogo).c_str(), src->details().c_str(),
	      src->op_format().c_str());

  if (dst->is_indirect())
    // Lose track of the dereference.
    dst = this->context_->sink();
  else if (dst->expr() != NULL)
    {
      // Analyze the lhs of the assignment.
      // Replace DST with this->context_->sink() if we can't track it.
      Expression* e = dst->expr();
      switch (e->classification())
        {
	case Expression::EXPRESSION_VAR_REFERENCE:
	  {
	    // If DST is a global variable, we have no way to track it.
	    Named_object* var = e->var_expression()->named_object();
	    if (var->is_variable() && var->var_value()->is_global())
	      dst = this->context_->sink();
	  }
	  break;

	case Expression::EXPRESSION_FIELD_REFERENCE:
	  {
	    Expression* strct = e->field_reference_expression()->expr();
	    if (strct->heap_expression() != NULL)
	      {
		// When accessing the field of a struct reference, we lose
		// track of the indirection.
		dst = this->context_->sink();
		break;
	      }

	    // Treat DST.x = SRC as if it were DST = SRC.
	    Node* struct_node = Node::make_node(strct);
	    this->assign(struct_node, src);
	    return;
	  }

	case Expression::EXPRESSION_ARRAY_INDEX:
	  {
	    Array_index_expression* are = e->array_index_expression();
	    if (!are->array()->type()->is_slice_type())
	      {
		// Treat DST[i] = SRC as if it were DST = SRC if DST if a fixed
		// array.
		Node* array_node = Node::make_node(are->array());
		this->assign(array_node, src);
		return;
	      }

	    // Lose track of the slice dereference.
	    dst = this->context_->sink();
	  }
	  break;

	case Expression::EXPRESSION_UNARY:
	  // Lose track of the dereference.
	  if (e->unary_expression()->op() == OPERATOR_MULT)
	    dst = this->context_->sink();
	  break;

	case Expression::EXPRESSION_MAP_INDEX:
	  {
	    // Lose track of the map's key and value.
	    Expression* index = e->map_index_expression()->index();
	    Node* index_node = Node::make_node(index);
	    this->assign(this->context_->sink(), index_node);

	    dst = this->context_->sink();
	  }
	  break;

        case Expression::EXPRESSION_TEMPORARY_REFERENCE:
          {
            // Temporary is tracked through the underlying Temporary_statement.
            Statement* t = dst->expr()->temporary_reference_expression()->statement();
            dst = Node::make_node(t);
          }
          break;

	default:
	  // TODO(cmang): Add debugging info here: only a few expressions
	  // should leave DST unmodified.
	  break;
        }
    }

  if (src->object() != NULL)
    this->flows(dst, src);
  else if (src->is_indirect())
    this->flows(dst, src);
  else if (src->expr() != NULL)
    {
      Expression* e = src->expr();
      switch (e->classification())
        {
	case Expression::EXPRESSION_VAR_REFERENCE:
        case Expression::EXPRESSION_ENCLOSED_VAR_REFERENCE:
	  // DST = var
	case Expression::EXPRESSION_HEAP:
	  // DST = &T{...}.
	case Expression::EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
	case Expression::EXPRESSION_SLICE_CONSTRUCTION:
	  // DST = [...]T{...}.
	case Expression::EXPRESSION_MAP_CONSTRUCTION:
	  // DST = map[T]V{...}.
	case Expression::EXPRESSION_STRUCT_CONSTRUCTION:
	  // DST = T{...}.
	case Expression::EXPRESSION_ALLOCATION:
	  // DST = new(T).
	case Expression::EXPRESSION_BOUND_METHOD:
	  // DST = x.M.
        case Expression::EXPRESSION_STRING_CONCAT:
          // DST = str1 + str2
	  this->flows(dst, src);
	  break;

	case Expression::EXPRESSION_UNSAFE_CONVERSION:
	  {
	    Expression* underlying = e->unsafe_conversion_expression()->expr();
	    Node* underlying_node = Node::make_node(underlying);
	    this->assign(dst, underlying_node);
	  }
	  break;

	case Expression::EXPRESSION_CALL:
	  {
	    Call_expression* call = e->call_expression();
            if (call->is_builtin())
              {
                Builtin_call_expression* bce = call->builtin_call_expression();
                if (bce->code() == Builtin_call_expression::BUILTIN_APPEND)
                  {
                    // Append returns the first argument.
                    // The subsequent arguments are already leaked because
                    // they are operands to append.
                    Node* appendee = Node::make_node(call->args()->front());
                    this->assign(dst, appendee);
                  }
                break;
              }
	    Func_expression* fe = call->fn()->func_expression();
	    if (fe != NULL && fe->is_runtime_function())
	      {
		switch (fe->runtime_code())
		  {
		  case Runtime::MAKECHAN:
		  case Runtime::MAKECHAN64:
		  case Runtime::MAKEMAP:
		  case Runtime::MAKESLICE:
		  case Runtime::MAKESLICE64:
		    // DST = make(...).
                    this->flows(dst, src);
                    break;

		  default:
		    break;
		  }
		break;
	      }
	    else if (fe != NULL
		     && fe->named_object()->is_function()
		     && fe->named_object()->func_value()->is_method()
		     && (call->is_deferred()
			 || call->is_concurrent()))
	      {
		// For a method call thunk, lose track of the call and treat it
		// as if DST = RECEIVER.
		Node* rcvr_node = Node::make_node(call->args()->front());
		this->assign(dst, rcvr_node);
		break;
	      }

            // Result flows to dst.
            Node* call_node = Node::make_node(e);
            Node::Escape_state* call_state = call_node->state(this->context_, NULL);
            std::vector<Node*> retvals = call_state->retvals;
            for (std::vector<Node*>::const_iterator p = retvals.begin();
                 p != retvals.end();
                 ++p)
              this->flows(dst, *p);
	  }
	  break;

        case Expression::EXPRESSION_CALL_RESULT:
          {
            Call_result_expression* cre = e->call_result_expression();
            Call_expression* call = cre->call()->call_expression();
            if (call->is_builtin())
              break;
            if (call->fn()->func_expression() != NULL
                && call->fn()->func_expression()->is_runtime_function())
              {
                switch (call->fn()->func_expression()->runtime_code())
                  {
                  case Runtime::IFACEE2E2:
                  case Runtime::IFACEI2E2:
                  case Runtime::IFACEE2I2:
                  case Runtime::IFACEI2I2:
                  case Runtime::IFACEE2T2P:
                  case Runtime::IFACEI2T2P:
                    {
                      // x, ok = v.(T), where T is a pointer or interface,
                      // is lowered to
                      // x, ok = IFACEI2E2(v), or
                      // x, ok = IFACEI2I2(type, v)
                      // The last arg flows to the first result.
                      // Note: IFACEX2T2 has different signature, handled by
                      // ::expression.
                      if (cre->index() != 0)
                        break;
                      Node* arg_node = Node::make_node(call->args()->back());
                      this->assign(dst, arg_node);
                    }
                    break;

                  default:
                    break;
                  }
                break;
              }

            Node* call_node = Node::make_node(call);
            Node* ret_node = call_node->state(context_, NULL)->retvals[cre->index()];
            this->assign(dst, ret_node);
          }
          break;

	case Expression::EXPRESSION_FUNC_REFERENCE:
	  if (e->func_expression()->closure() != NULL)
            this->flows(dst, src);
	  break;

        case Expression::EXPRESSION_CONVERSION:
          {
            Type_conversion_expression* tce = e->conversion_expression();
            Type* ft = tce->expr()->type();
            Type* tt = tce->type();
            if ((ft->is_string_type() && tt->is_slice_type())
                || (ft->is_slice_type() && tt->is_string_type())
                || (ft->integer_type() != NULL && tt->is_string_type()))
              {
                // string([]byte), string([]rune), []byte(string), []rune(string), string(rune)
                this->flows(dst, src);
                break;
              }
            // Conversion preserves input value.
            Expression* underlying = tce->expr();
            this->assign(dst, Node::make_node(underlying));
          }
          break;

	case Expression::EXPRESSION_FIELD_REFERENCE:
	  {
	    // A non-pointer can't escape from a struct.
	    if (!e->type()->has_pointer())
	      break;
	  }
	  // Fall through.

	case Expression::EXPRESSION_TYPE_GUARD:
	case Expression::EXPRESSION_ARRAY_INDEX:
	case Expression::EXPRESSION_STRING_INDEX:
	  {
	    Expression* left = NULL;
	    if (e->field_reference_expression() != NULL)
	      {
		left = e->field_reference_expression()->expr();
		if (left->unary_expression() != NULL
		    && left->unary_expression()->op() == OPERATOR_MULT)
		  {
		    // DST = (*x).f
		    this->flows(dst, src);
		    break;
		  }
	      }
	    else if (e->type_guard_expression() != NULL)
	      left = e->type_guard_expression()->expr();
	    else if (e->array_index_expression() != NULL)
	      {
		Array_index_expression* aie = e->array_index_expression();
		if (aie->end() != NULL)
                  // slicing
                  if (aie->array()->type()->is_slice_type())
                    left = aie->array();
                  else
                    {
                      // slicing an array
                      // The gc compiler has an implicit address operator.
                      go_assert(src->child() != NULL);
                      this->assign(dst, src->child());
                      break;
                    }
		else if (!aie->array()->type()->is_slice_type())
		  {
		    // Indexing an array preserves the input value.
		    Node* array_node = Node::make_node(aie->array());
		    this->assign(dst, array_node);
		    break;
		  }
		else
		  {
		    this->flows(dst, src);
		    break;
		  }
	      }
	    else if (e->string_index_expression() != NULL)
	      {
		String_index_expression* sie = e->string_index_expression();
		if (e->type()->is_string_type())
                  // slicing
		  left = sie->string();
		else
		  {
		    this->flows(dst, src);
		    break;
		  }
	      }
	    go_assert(left != NULL);

	    // Conversions, field access, and slicing all preserve the input
	    // value.
	    Node* left_node = Node::make_node(left);
	    this->assign(dst, left_node);
	  }
	  break;

	case Expression::EXPRESSION_BINARY:
	  {
	    switch (e->binary_expression()->op())
	      {
	      case OPERATOR_PLUS:
	      case OPERATOR_MINUS:
	      case OPERATOR_XOR:
              case OPERATOR_OR:
	      case OPERATOR_MULT:
	      case OPERATOR_DIV:
	      case OPERATOR_MOD:
	      case OPERATOR_LSHIFT:
	      case OPERATOR_RSHIFT:
	      case OPERATOR_AND:
	      case OPERATOR_BITCLEAR:
		{
		  Node* left = Node::make_node(e->binary_expression()->left());
		  this->assign(dst, left);
		  Node* right = Node::make_node(e->binary_expression()->right());
		  this->assign(dst, right);
		}
		break;

	      default:
		break;
	      }
	  }
	  break;

	case Expression::EXPRESSION_UNARY:
	  {
	    switch (e->unary_expression()->op())
	      {
	      case OPERATOR_PLUS:
	      case OPERATOR_MINUS:
	      case OPERATOR_XOR:
		{
		    Node* op_node =
		      Node::make_node(e->unary_expression()->operand());
		    this->assign(dst, op_node);
		}
		break;

	      case OPERATOR_MULT:
		// DST = *x
	      case OPERATOR_AND:
		// DST = &x
		this->flows(dst, src);
		break;

	      default:
		break;
	      }
	  }
	  break;

	case Expression::EXPRESSION_TEMPORARY_REFERENCE:
	  {
	    Statement* temp = e->temporary_reference_expression()->statement();
            this->assign(dst, Node::make_node(temp));
	  }
	  break;

	default:
	  // TODO(cmang): Add debug info here; this should not be reachable.
	  // For now, just to be conservative, we'll just say dst flows to src.
	  break;
	}
    }
  else if (src->statement() != NULL && src->statement()->temporary_statement() != NULL)
    this->flows(dst, src);
}

// Model the assignment of DST to an indirection of SRC.

void
Escape_analysis_assign::assign_deref(Node* dst, Node* src)
{
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  // No need to try indirections on literal values
	  // or numeric constants.
	  return;

	default:
	  break;
        }
    }

  this->assign(dst, this->context_->add_dereference(src));
}

// Model the input-to-output assignment flow of one of a function call's
// arguments, where the flow is encoded in NOTE.

int
Escape_analysis_assign::assign_from_note(std::string* note,
					 const std::vector<Node*>& dsts,
					 Node* src)
{
  int enc = Escape_note::parse_tag(note);
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  // There probably isn't a note for a literal value.  Literal values
	  // usually don't escape unless we lost track of the value some how.
	  return enc;

	default:
	  break;
        }
    }

  if (this->context_->gogo()->debug_escape_level() > 2)
    go_inform(src->location(), "assignfromtag:: src=%s em=%s",
              src->ast_format(context_->gogo()).c_str(),
	      Escape_note::make_tag(enc).c_str());

  if (enc == Node::ESCAPE_UNKNOWN)
    {
      // Lost track of the value.
      this->assign(this->context_->sink(), src);
      return enc;
    }
  else if (enc == Node::ESCAPE_NONE)
    return enc;

  // If the content inside a parameter (reached via indirection) escapes to
  // the heap, mark it.
  if ((enc & ESCAPE_CONTENT_ESCAPES) != 0)
    this->assign(this->context_->sink(), this->context_->add_dereference(src));

  int save_enc = enc;
  enc >>= ESCAPE_RETURN_BITS;
  for (std::vector<Node*>::const_iterator p = dsts.begin();
       enc != 0 && p != dsts.end();
       ++p)
    {
      // Prefer the lowest-level path to the reference (for escape purposes).
      // Two-bit encoding (for example. 1, 3, and 4 bits are other options)
      //  01 = 0-level
      //  10 = 1-level, (content escapes),
      //  11 = 2-level, (content of content escapes).
      int bits = enc & ESCAPE_BITS_MASK_FOR_TAG;
      if (bits > 0)
	{
	  Node* n = src;
	  for (int i = 0; i < bits - 1; ++i)
	    {
	      // Encode level > 0 as indirections.
	      n = this->context_->add_dereference(n);
	    }
	  this->assign(*p, n);
	}
      enc >>= ESCAPE_BITS_PER_OUTPUT_IN_TAG;
    }

  // If there are too many outputs to fit in the tag, that is handled on the
  // encoding end as ESCAPE_HEAP, so there is no need to check here.
  return save_enc;
}

// Record the flow of SRC to DST in DST.

void
Escape_analysis_assign::flows(Node* dst, Node* src)
{
  // Don't bother capturing the flow from scalars.
  if (src->type() != NULL && !src->type()->has_pointer())
    return;

  // Don't confuse a blank identifier with the sink.
  if (dst->is_sink() && dst != this->context_->sink())
    return;

  Node::Escape_state* dst_state = dst->state(this->context_, NULL);
  Node::Escape_state* src_state = src->state(this->context_, NULL);
  if (dst == src
      || dst_state == src_state
      || dst_state->flows.find(src) != dst_state->flows.end())
    return;

  Gogo* gogo = this->context_->gogo();
  if (gogo->debug_escape_level() > 2)
    go_inform(Linemap::unknown_location(), "flows:: %s <- %s",
              dst->ast_format(gogo).c_str(), src->ast_format(gogo).c_str());

  if (dst_state->flows.empty())
    this->context_->add_dst(dst);

  dst_state->flows.insert(src);
}

// Build a connectivity graph between nodes in the function being analyzed.

void
Gogo::assign_connectivity(Escape_context* context, Named_object* fn)
{
  // Must be defined outside of this package.
  if (!fn->is_function())
    return;

  int save_depth = context->loop_depth();
  context->set_loop_depth(1);

  Escape_analysis_assign ea(context, fn);
  Function::Results* res = fn->func_value()->result_variables();
  if (res != NULL)
    {
      for (Function::Results::const_iterator p = res->begin();
	   p != res->end();
	   ++p)
	{
	  Node* res_node = Node::make_node(*p);
	  Node::Escape_state* res_state = res_node->state(context, fn);
          res_state->fn = fn;
	  res_state->loop_depth = 0;

	  // If this set of functions is recursive, we lose track of the return values.
	  // Just say that the result flows to the sink.
	  if (context->recursive())
	    ea.flows(context->sink(), res_node);
	}
    }

  const Bindings* callee_bindings = fn->func_value()->block()->bindings();
  Function_type* fntype = fn->func_value()->type();
  Typed_identifier_list* params = (fntype->parameters() != NULL
				   ? fntype->parameters()->copy()
				   : new Typed_identifier_list);
  if (fntype->receiver() != NULL)
    params->push_back(*fntype->receiver());

  for (Typed_identifier_list::const_iterator p = params->begin();
       p != params->end();
       ++p)
    {
      if (p->name().empty() || Gogo::is_sink_name(p->name()))
	continue;

      Named_object* param_no = callee_bindings->lookup_local(p->name());
      go_assert(param_no != NULL);
      Node* param_node = Node::make_node(param_no);
      Node::Escape_state* param_state = param_node->state(context, fn);
      param_state->fn = fn;
      param_state->loop_depth = 1;

      if (!p->type()->has_pointer())
        continue;

      // External function?  Parameters must escape unless //go:noescape is set.
      // TODO(cmang): Implement //go:noescape directive.
      if (fn->package() != NULL)
	param_node->set_encoding(Node::ESCAPE_HEAP);
      else
        {
          param_node->set_encoding(Node::ESCAPE_NONE);
          context->track(param_node);
        }
    }

  Escape_analysis_loop el;
  fn->func_value()->traverse(&el);

  fn->func_value()->traverse(&ea);
  context->set_loop_depth(save_depth);
}

class Escape_analysis_flood
{
 public:
  Escape_analysis_flood(Escape_context* context)
    : context_(context)
  { }

  // Use the escape information in dst to update the escape information in src
  // and src's upstream.
  void
  flood(Level, Node* dst, Node* src, int);

 private:
  // The escape context for the group of functions being flooded.
  Escape_context* context_;
};

// Whenever we hit a dereference node, the level goes up by one, and whenever
// we hit an address-of, the level goes down by one. as long as we're on a
// level > 0 finding an address-of just means we're following the upstream
// of a dereference, so this address doesn't leak (yet).
// If level == 0, it means the /value/ of this node can reach the root of this
// flood so if this node is an address-of, its argument should be marked as
// escaping iff its current function and loop depth are different from the
// flood's root.
// Once an object has been moved to the heap, all of its upstream should be
// considered escaping to the global scope.
// This is an implementation of gc/esc.go:escwalkBody.

void
Escape_analysis_flood::flood(Level level, Node* dst, Node* src,
			     int extra_loop_depth)
{
  // No need to flood src if it is a literal.
  if (src->expr() != NULL)
    {
      switch (src->expr()->classification())
        {
	case Expression::EXPRESSION_BOOLEAN:
	case Expression::EXPRESSION_STRING:
	case Expression::EXPRESSION_INTEGER:
	case Expression::EXPRESSION_FLOAT:
	case Expression::EXPRESSION_COMPLEX:
	case Expression::EXPRESSION_NIL:
	case Expression::EXPRESSION_IOTA:
	  return;

	default:
	  break;
        }
    }

  Node::Escape_state* src_state = src->state(this->context_, NULL);
  if (src_state->flood_id == this->context_->flood_id())
    {
      // Esclevels are vectors, do not compare as integers,
      // and must use "min" of old and new to guarantee
      // convergence.
      level = level.min(src_state->level);
      if (level == src_state->level)
	{
	  // Have we been here already with an extraloopdepth,
	  // or is the extraloopdepth provided no improvement on
	  // what's already been seen?
	  if (src_state->max_extra_loop_depth >= extra_loop_depth
	      || src_state->loop_depth >= extra_loop_depth)
	    return;
	  src_state->max_extra_loop_depth = extra_loop_depth;
	}
    }
  else
    src_state->max_extra_loop_depth = -1;

  src_state->flood_id = this->context_->flood_id();
  src_state->level = level;
  int mod_loop_depth = std::max(extra_loop_depth, src_state->loop_depth);

  Gogo* gogo = this->context_->gogo();
  int debug_level = gogo->debug_escape_level();
  if (debug_level > 1)
    go_inform(Linemap::unknown_location(),
	      "escwalk: level:{%d %d} depth:%d "
	      "op=%s %s(%s) "
	      "scope:%s[%d] "
	      "extraloopdepth=%d",
	      level.value(), level.suffix_value(), this->context_->pdepth(),
	      src->op_format().c_str(),
	      src->ast_format(gogo).c_str(),
	      src->details().c_str(),
	      debug_function_name(src_state->fn).c_str(),
	      src_state->loop_depth,
	      extra_loop_depth);

  this->context_->increase_pdepth();

  // Input parameter flowing into output parameter?
  Named_object* src_no = NULL;
  if (src->expr() != NULL && src->expr()->var_expression() != NULL)
    src_no = src->expr()->var_expression()->named_object();
  else
    src_no = src->object();
  bool src_is_param = (src_no != NULL
		       && src_no->is_variable()
		       && src_no->var_value()->is_parameter());

  Named_object* dst_no = NULL;
  if (dst->expr() != NULL && dst->expr()->var_expression() != NULL)
    dst_no = dst->expr()->var_expression()->named_object();
  else
    dst_no = dst->object();
  bool dst_is_result = dst_no != NULL && dst_no->is_result_variable();
  Node::Escape_state* dst_state = dst->state(this->context_, NULL);

  if (src_is_param
      && dst_is_result
      && src_state->fn == dst_state->fn
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_HEAP)
      && dst->encoding() != Node::ESCAPE_HEAP)
    {
      // This case handles:
      // 1. return in
      // 2. return &in
      // 3. tmp := in; return &tmp
      // 4. return *in
      if (debug_level != 0)
	{
	  if (debug_level == 1)
	    go_inform(src->definition_location(),
		      "leaking param: %s to result %s level=%d",
		      src->ast_format(gogo).c_str(),
		      dst->ast_format(gogo).c_str(),
		      level.value());
	  else
	    go_inform(src->definition_location(),
		      "leaking param: %s to result %s level={%d %d}",
		      src->ast_format(gogo).c_str(),
		      dst->ast_format(gogo).c_str(),
		      level.value(), level.suffix_value());
	}

      if ((src->encoding() & ESCAPE_MASK) != Node::ESCAPE_RETURN)
	{
	  int enc =
	    Node::ESCAPE_RETURN | (src->encoding() & ESCAPE_CONTENT_ESCAPES);
	  src->set_encoding(enc);
	}

      int enc = Node::note_inout_flows(src->encoding(),
				       dst_no->result_var_value()->index(),
				       level);
      src->set_encoding(enc);

      // In gc/esc.go:escwalkBody, this is a goto to the label for recursively
      // flooding the connection graph.  Inlined here for convenience.
      level = level.copy();
      for (std::set<Node*>::const_iterator p = src_state->flows.begin();
	   p != src_state->flows.end();
	   ++p)
	this->flood(level, dst, *p, extra_loop_depth);
      return;
    }

  // If parameter content escape to heap, set ESCAPE_CONTENT_ESCAPES.
  // Note minor confusion around escape from pointer-to-struct vs
  // escape from struct.
  if (src_is_param
      && dst->encoding() == Node::ESCAPE_HEAP
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_HEAP)
      && level.value() > 0)
    {
      int enc =
	Node::max_encoding((src->encoding() | ESCAPE_CONTENT_ESCAPES),
			   Node::ESCAPE_NONE);
      src->set_encoding(enc);
      if (debug_level != 0)
	go_inform(src->definition_location(), "mark escaped content: %s",
		  src->ast_format(gogo).c_str());
    }

  // A src object leaks if its value or address is assigned to a dst object
  // in a different scope (at a different loop depth).
  bool src_leaks = (level.value() <= 0
		    && level.suffix_value() <= 0
		    && dst_state->loop_depth < mod_loop_depth);
  src_leaks = src_leaks || (level.value() <= 0
              && (dst->encoding() & ESCAPE_MASK) == Node::ESCAPE_HEAP);
  // old src encoding, used to prevent duplicate error messages
  int osrcesc = src->encoding();

  if (src_is_param
      && (src_leaks || dst_state->loop_depth < 0)
      && (src->encoding() & ESCAPE_MASK) < int(Node::ESCAPE_HEAP))
    {
      if (level.suffix_value() > 0)
	{
	  int enc =
	    Node::max_encoding((src->encoding() | ESCAPE_CONTENT_ESCAPES),
			       Node::ESCAPE_NONE);
	  src->set_encoding(enc);
	  if (debug_level != 0 && osrcesc != src->encoding())
	    go_inform(src->definition_location(), "leaking param content: %s",
		      src->ast_format(gogo).c_str());
	}
      else
	{
	  if (debug_level != 0)
	    go_inform(src->definition_location(), "leaking param: %s",
                      src->ast_format(gogo).c_str());
	  src->set_encoding(Node::ESCAPE_HEAP);
	}
    }
  else if (src->expr() != NULL)
    {
      Expression* e = src->expr();
      if (e->enclosed_var_expression() != NULL)
	{
	  if (src_leaks && debug_level != 0)
	    go_inform(src->location(), "leaking closure reference %s",
		      src->ast_format(gogo).c_str());

	  Node* enclosed_node =
	    Node::make_node(e->enclosed_var_expression()->variable());
	  this->flood(level, dst, enclosed_node, -1);
	}
      else if (e->heap_expression() != NULL
	  || (e->unary_expression() != NULL
	      && e->unary_expression()->op() == OPERATOR_AND))
	{
	  // Pointer literals and address-of expressions.
	  Expression* underlying;
	  if (e->heap_expression())
	    underlying = e->heap_expression()->expr();
	  else
	    underlying = e->unary_expression()->operand();
	  Node* underlying_node = Node::make_node(underlying);

	  // If the address leaks, the underyling object must be moved
	  // to the heap.
	  underlying->address_taken(src_leaks);
	  if (src_leaks)
	    {
	      src->set_encoding(Node::ESCAPE_HEAP);
              if (osrcesc != src->encoding())
                {
                  move_to_heap(gogo, underlying);
                  if (debug_level > 1)
                    go_inform(src->location(),
                              "%s escapes to heap, level={%d %d}, "
                              "dst.eld=%d, src.eld=%d",
                              src->ast_format(gogo).c_str(), level.value(),
                              level.suffix_value(), dst_state->loop_depth,
                              mod_loop_depth);
                  else if (debug_level > 0)
                    go_inform(src->location(), "%s escapes to heap",
                              src->ast_format(gogo).c_str());
                }

	      this->flood(level.decrease(), dst,
			  underlying_node, mod_loop_depth);
	      extra_loop_depth = mod_loop_depth;
	    }
	  else
	    {
	      // Decrease the level each time we take the address of the object.
	      this->flood(level.decrease(), dst, underlying_node, -1);
	    }
	}
      else if (e->slice_literal() != NULL)
	{
	  Slice_construction_expression* slice = e->slice_literal();
	  if (slice->vals() != NULL)
	    {
	      for (Expression_list::const_iterator p = slice->vals()->begin();
		   p != slice->vals()->end();
		   ++p)
		{
		  if ((*p) != NULL)
		    this->flood(level.decrease(), dst, Node::make_node(*p), -1);
		}
	    }
	  if (src_leaks)
	    {
	      src->set_encoding(Node::ESCAPE_HEAP);
	      if (debug_level != 0 && osrcesc != src->encoding())
		go_inform(src->location(), "%s escapes to heap",
			  src->ast_format(gogo).c_str());
	      extra_loop_depth = mod_loop_depth;
	    }
	}
      else if (e->call_expression() != NULL)
	{
	  Call_expression* call = e->call_expression();
          if (call->is_builtin())
            {
              Builtin_call_expression* bce = call->builtin_call_expression();
              if (bce->code() == Builtin_call_expression::BUILTIN_APPEND)
                {
                  // Propagate escape information to appendee.
                  Expression* appendee = call->args()->front();
                  this->flood(level, dst, Node::make_node(appendee), -1);
                }
            }
          else if (call->fn()->func_expression() != NULL
                   && call->fn()->func_expression()->is_runtime_function())
            {
              switch (call->fn()->func_expression()->runtime_code())
                {
                case Runtime::MAKECHAN:
		case Runtime::MAKECHAN64:
                case Runtime::MAKEMAP:
                case Runtime::MAKESLICE:
                case Runtime::MAKESLICE64:
                  if (src_leaks)
                    {
                      src->set_encoding(Node::ESCAPE_HEAP);
                      if (debug_level != 0 && osrcesc != src->encoding())
                        go_inform(src->location(), "%s escapes to heap",
                                  src->ast_format(gogo).c_str());
                      extra_loop_depth = mod_loop_depth;
                    }
                  break;

                default:
                  break;
                }
            }
          else if (src_state->retvals.size() > 0)
            {
              // In this case a link went directly to a call, but should really go
              // to the dummy .outN outputs that were created for the call that
              // themselves link to the inputs with levels adjusted.
              // See e.g. #10466.
              // This can only happen with functions returning a single result.
              go_assert(src_state->retvals.size() == 1);
              if (debug_level > 2)
                go_inform(src->location(), "[%d] dst %s escwalk replace src: %s with %s",
                          this->context_->loop_depth(),
                          dst->ast_format(gogo).c_str(),
                          src->ast_format(gogo).c_str(),
                          src_state->retvals[0]->ast_format(gogo).c_str());
              src = src_state->retvals[0];
              src_state = src->state(this->context_, NULL);
            }
	}
      else if (e->allocation_expression() != NULL && src_leaks)
	{
	  // Calls to Runtime::NEW get lowered into an allocation expression.
	  src->set_encoding(Node::ESCAPE_HEAP);
	  if (debug_level != 0 && osrcesc != src->encoding())
	    go_inform(src->location(), "%s escapes to heap",
                      src->ast_format(gogo).c_str());
          extra_loop_depth = mod_loop_depth;
	}
      else if ((e->map_literal() != NULL
               || e->string_concat_expression() != NULL
               || (e->func_expression() != NULL && e->func_expression()->closure() != NULL)
               || e->bound_method_expression() != NULL)
               && src_leaks)
        {
          src->set_encoding(Node::ESCAPE_HEAP);
          if (debug_level != 0 && osrcesc != src->encoding())
            go_inform(src->location(), "%s escapes to heap",
                      src->ast_format(gogo).c_str());
          extra_loop_depth = mod_loop_depth;
        }
      else if (e->conversion_expression() != NULL && src_leaks)
        {
          Type_conversion_expression* tce = e->conversion_expression();
          Type* ft = tce->expr()->type();
          Type* tt = tce->type();
          if ((ft->is_string_type() && tt->is_slice_type())
              || (ft->is_slice_type() && tt->is_string_type())
              || (ft->integer_type() != NULL && tt->is_string_type()))
            {
              // string([]byte), string([]rune), []byte(string), []rune(string), string(rune)
              src->set_encoding(Node::ESCAPE_HEAP);
              if (debug_level != 0 && osrcesc != src->encoding())
                go_inform(src->location(), "%s escapes to heap",
                          src->ast_format(gogo).c_str());
              extra_loop_depth = mod_loop_depth;
            }
        }
      else if (e->array_index_expression() != NULL
               && !e->array_index_expression()->array()->type()->is_slice_type())
        {
          Array_index_expression* aie = e->array_index_expression();
          if (aie->end() != NULL)
            {
              // Slicing an array.
              // Flow its implicit address-of node to DST.
              this->flood(level, dst, src->child(), -1);
            }
          else
            {
              // Indexing an array.
              // An array element flowing to DST behaves like the array
              // flowing to DST.
              Expression* underlying = e->array_index_expression()->array();
              Node* underlying_node = Node::make_node(underlying);
              this->flood(level, dst, underlying_node, -1);
            }
        }
      else if ((e->field_reference_expression() != NULL
		&& e->field_reference_expression()->expr()->unary_expression() == NULL)
	       || e->type_guard_expression() != NULL
	       || (e->array_index_expression() != NULL
		   && e->array_index_expression()->end() != NULL)
	       || (e->string_index_expression() != NULL
		   && e->type()->is_string_type()))
	{
	  Expression* underlying;
	  if (e->field_reference_expression() != NULL)
	    underlying = e->field_reference_expression()->expr();
	  else if (e->type_guard_expression() != NULL)
	    underlying = e->type_guard_expression()->expr();
	  else if (e->array_index_expression() != NULL)
	    underlying = e->array_index_expression()->array();
	  else
	    underlying = e->string_index_expression()->string();

	  Node* underlying_node = Node::make_node(underlying);
	  this->flood(level, dst, underlying_node, -1);
	}
      else if ((e->field_reference_expression() != NULL
		&& e->field_reference_expression()->expr()->unary_expression() != NULL)
	       || e->array_index_expression() != NULL
	       || e->map_index_expression() != NULL
	       || (e->unary_expression() != NULL
		   && e->unary_expression()->op() == OPERATOR_MULT))
	{
	  Expression* underlying;
	  if (e->field_reference_expression() != NULL)
	    {
	      underlying = e->field_reference_expression()->expr();
	      underlying = underlying->unary_expression()->operand();
	    }
	  else if (e->array_index_expression() != NULL)
	    underlying = e->array_index_expression()->array();
	  else if (e->map_index_expression() != NULL)
	    underlying = e->map_index_expression()->map();
	  else
	    underlying = e->unary_expression()->operand();

	  // Increase the level for a dereference.
	  Node* underlying_node = Node::make_node(underlying);
	  this->flood(level.increase(), dst, underlying_node, -1);
	}
      else if (e->temporary_reference_expression() != NULL)
        {
          Statement* t = e->temporary_reference_expression()->statement();
          this->flood(level, dst, Node::make_node(t), -1);
        }
    }
  else if (src->is_indirect())
    // Increase the level for a dereference.
    this->flood(level.increase(), dst, src->child(), -1);

  level = level.copy();
  for (std::set<Node*>::const_iterator p = src_state->flows.begin();
       p != src_state->flows.end();
       ++p)
    this->flood(level, dst, *p, extra_loop_depth);

  this->context_->decrease_pdepth();
}

// Propagate escape information across the nodes modeled in this Analysis_set.
// This is an implementation of gc/esc.go:escflood.

void
Gogo::propagate_escape(Escape_context* context, Node* dst)
{
  if (dst->object() == NULL
      && (dst->expr() == NULL
          || (dst->expr()->var_expression() == NULL
              && dst->expr()->enclosed_var_expression() == NULL
              && dst->expr()->func_expression() == NULL)))
    return;

  Node::Escape_state* state = dst->state(context, NULL);
  Gogo* gogo = context->gogo();
  if (gogo->debug_escape_level() > 1)
    go_inform(Linemap::unknown_location(), "escflood:%d: dst %s scope:%s[%d]",
	      context->flood_id(), dst->ast_format(gogo).c_str(),
	      debug_function_name(state->fn).c_str(),
	      state->loop_depth);

  Escape_analysis_flood eaf(context);
  for (std::set<Node*>::const_iterator p = state->flows.begin();
       p != state->flows.end();
       ++p)
    {
      context->increase_flood_id();
      eaf.flood(Level::From(0), dst, *p, -1);
    }
}

class Escape_analysis_tag
{
 public:
  Escape_analysis_tag(Escape_context* context)
    : context_(context)
  { }

  // Add notes to the function's type about the escape information of its
  // input parameters.
  void
  tag(Named_object* fn);

 private:
  Escape_context* context_;
};

void
Escape_analysis_tag::tag(Named_object* fn)
{
  // External functions are assumed unsafe
  // unless //go:noescape is given before the declaration.
  if (fn->package() != NULL)
    return;

  if (fn->is_function_declaration())
    {
      Function_declaration* fdcl = fn->func_declaration_value();
      if ((fdcl->pragmas() & GOPRAGMA_NOESCAPE) != 0)
        {
          Function_type* fntype = fdcl->type();
          if (fntype->parameters() != NULL)
            {
              const Typed_identifier_list* til = fntype->parameters();
              int i = 0;
              for (Typed_identifier_list::const_iterator p = til->begin();
                   p != til->end();
                   ++p, ++i)
                if (p->type()->has_pointer())
                  fntype->add_parameter_note(i, Node::ESCAPE_NONE);
            }
        }
    }

  if (!fn->is_function())
    return;

  Function_type* fntype = fn->func_value()->type();
  Bindings* bindings = fn->func_value()->block()->bindings();

  if (fntype->is_method())
    {
      if (fntype->receiver()->name().empty()
          || Gogo::is_sink_name(fntype->receiver()->name()))
        // Unnamed receiver is not used in the function body, does not escape.
        fntype->add_receiver_note(Node::ESCAPE_NONE);
      else
        {
          Named_object* rcvr_no = bindings->lookup(fntype->receiver()->name());
          go_assert(rcvr_no != NULL);
          Node* rcvr_node = Node::make_node(rcvr_no);
          switch ((rcvr_node->encoding() & ESCAPE_MASK))
            {
            case Node::ESCAPE_NONE: // not touched by flood
            case Node::ESCAPE_RETURN:
              if (fntype->receiver()->type()->has_pointer())
                // Don't bother tagging for scalars.
                fntype->add_receiver_note(rcvr_node->encoding());
              break;

            case Node::ESCAPE_HEAP: // flooded, moved to heap.
              break;

            default:
              break;
            }
        }
    }

  int i = 0;
  if (fntype->parameters() != NULL)
    {
      const Typed_identifier_list* til = fntype->parameters();
      for (Typed_identifier_list::const_iterator p = til->begin();
	   p != til->end();
	   ++p, ++i)
	{
	  if (p->name().empty() || Gogo::is_sink_name(p->name()))
            {
              // Parameter not used in the function body, does not escape.
              if (p->type()->has_pointer())
                fntype->add_parameter_note(i, Node::ESCAPE_NONE);
              continue;
            }

	  Named_object* param_no = bindings->lookup(p->name());
	  go_assert(param_no != NULL);
	  Node* param_node = Node::make_node(param_no);
	  switch ((param_node->encoding() & ESCAPE_MASK))
	    {
	    case Node::ESCAPE_NONE: // not touched by flood
	    case Node::ESCAPE_RETURN:
	      if (p->type()->has_pointer())
		// Don't bother tagging for scalars.
		fntype->add_parameter_note(i, param_node->encoding());
	      break;

	    case Node::ESCAPE_HEAP: // flooded, moved to heap.
	      break;

	    default:
	      break;
	    }
	}
    }
  fntype->set_is_tagged();
}

// Tag each top-level function with escape information that will be used to
// retain analysis results across imports.

void
Gogo::tag_function(Escape_context* context, Named_object* fn)
{
  Escape_analysis_tag eat(context);
  eat.tag(fn);
}

// Reclaim memory of escape analysis Nodes.

void
Gogo::reclaim_escape_nodes()
{
  Node::reclaim_nodes();
}

void
Node::reclaim_nodes()
{
  for (std::map<Named_object*, Node*>::iterator p = Node::objects.begin();
       p != Node::objects.end();
       ++p)
    delete p->second;
  Node::objects.clear();

  for (std::map<Expression*, Node*>::iterator p = Node::expressions.begin();
       p != Node::expressions.end();
       ++p)
    delete p->second;
  Node::expressions.clear();

  for (std::map<Statement*, Node*>::iterator p = Node::statements.begin();
       p != Node::statements.end();
       ++p)
    delete p->second;
  Node::statements.clear();

  for (std::vector<Node*>::iterator p = Node::indirects.begin();
       p != Node::indirects.end();
       ++p)
    delete *p;
  Node::indirects.clear();
}
