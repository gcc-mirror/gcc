// ast-dump.cc -- AST debug dump.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <iostream>
#include <fstream>
#include <sstream>

#include "gogo.h"
#include "expressions.h"
#include "statements.h"
#include "types.h"
#include "ast-dump.h"
#include "go-c.h"
#include "go-dump.h"
#include "go-diagnostics.h"

// The -fgo-dump-ast flag to activate AST dumps.

Go_dump ast_dump_flag("ast");

// This class is used to traverse the tree to look for blocks and
// function headers.

class Ast_dump_traverse_blocks_and_functions : public Traverse
{
 public:
  Ast_dump_traverse_blocks_and_functions(Ast_dump_context* ast_dump_context)
      : Traverse(traverse_blocks | traverse_functions | traverse_variables),
      ast_dump_context_(ast_dump_context)
  { }

 protected:
  int
  block(Block*);

  int
  function(Named_object*);

  int
  variable(Named_object*);

 private:
  Ast_dump_context* ast_dump_context_;
};

// This class is used to traverse the tree to look for statements.

class Ast_dump_traverse_statements : public Traverse
{
 public:
  Ast_dump_traverse_statements(Ast_dump_context* ast_dump_context)
      : Traverse(traverse_statements),
      ast_dump_context_(ast_dump_context)
  { }

 protected:
  int
  statement(Block*, size_t* pindex, Statement*);

 private:
  Ast_dump_context* ast_dump_context_;
};

// For each block we enclose it in brackets.

int Ast_dump_traverse_blocks_and_functions::block(Block * block)
{
  if (block == NULL)
    {
      this->ast_dump_context_->ostream() << std::endl;
      return TRAVERSE_EXIT;
    }

  this->ast_dump_context_->print_indent();
  this->ast_dump_context_->ostream() << "{" << std::endl;
  this->ast_dump_context_->indent();

  // Dump statememts.
  Ast_dump_traverse_statements adts(this->ast_dump_context_);
  block->traverse(&adts);

  this->ast_dump_context_->unindent();
  this->ast_dump_context_->print_indent();
  this->ast_dump_context_->ostream() << "}" << std::endl;

  return TRAVERSE_SKIP_COMPONENTS;
}

// Dump each traversed statement.

int
Ast_dump_traverse_statements::statement(Block* block, size_t* pindex,
                                        Statement* statement)
{
  statement->dump_statement(this->ast_dump_context_);

  if (statement->is_block_statement())
    {
      Ast_dump_traverse_blocks_and_functions adtbf(this->ast_dump_context_);
      statement->traverse(block, pindex, &adtbf);
    }

  return TRAVERSE_SKIP_COMPONENTS;
}

// Dump the function header.

int
Ast_dump_traverse_blocks_and_functions::function(Named_object* no)
{
  this->ast_dump_context_->ostream() << no->name();

  go_assert(no->is_function());
  Function* func = no->func_value();

  this->ast_dump_context_->ostream() << "(";
  this->ast_dump_context_->dump_typed_identifier_list(
                              func->type()->parameters());

  this->ast_dump_context_->ostream() << ")";

  Function::Results* res = func->result_variables();
  if (res != NULL && !res->empty())
    {
      this->ast_dump_context_->ostream() << " (";

      for (Function::Results::const_iterator it = res->begin();
          it != res->end();
          it++)
        {
          if (it != res->begin())
            this->ast_dump_context_->ostream() << ",";
          Named_object* rno = (*it);

          this->ast_dump_context_->ostream() << rno->name() << " ";
          go_assert(rno->is_result_variable());
          Result_variable* resvar = rno->result_var_value();

          this->ast_dump_context_->dump_type(resvar->type());

        }
      this->ast_dump_context_->ostream() << ")";
    }

  this->ast_dump_context_->ostream() << " : ";
  this->ast_dump_context_->dump_type(func->type());
  this->ast_dump_context_->ostream() << std::endl;

  return TRAVERSE_CONTINUE;
}

// Dump variable preinits

int
Ast_dump_traverse_blocks_and_functions::variable(Named_object* no)
{
  if (!no->is_variable())
    return TRAVERSE_CONTINUE;

  Variable* var = no->var_value();
  if (var->has_pre_init())
    {
      this->ast_dump_context_->ostream() << "// preinit block for var "
                                         << no->message_name() << "\n";
      var->preinit()->traverse(this);
    }

  return TRAVERSE_CONTINUE;
}



// Class Ast_dump_context.

Ast_dump_context::Ast_dump_context(std::ostream* out /* = NULL */,
				   bool dump_subblocks /* = true */)
  :  indent_(0), dump_subblocks_(dump_subblocks), ostream_(out), gogo_(NULL)
{
}

// Dump files will be named %basename%.dump.ast

const char* kAstDumpFileExtension = ".dump.ast";

// Dump the internal representation.

void
Ast_dump_context::dump(Gogo* gogo, const char* basename)
{
  std::ofstream out;
  std::string dumpname(basename);
  dumpname += ".dump.ast";
  out.open(dumpname.c_str());

  if (out.fail())
    {
      go_error_at(Linemap::unknown_location(),
		  "cannot open %s:%m; %<-fgo-dump-ast%> ignored",
		  dumpname.c_str());
      return;
    }

  this->gogo_ = gogo;
  this->ostream_ = &out;

  Ast_dump_traverse_blocks_and_functions adtbf(this);
  gogo->traverse(&adtbf);

  out.close();
}

// Dump a textual representation of a type to the
// the dump file.

void
Ast_dump_context::dump_type(const Type* t)
{
  if (t == NULL)
    this->ostream() << "(nil type)";
  else
    // FIXME: write a type pretty printer instead of
    // using mangled names.
    if (this->gogo_ != NULL)
      {
	Backend_name bname;
	t->backend_name(this->gogo_, &bname);
	this->ostream() << "(" << bname.name() << ")";
      }
}

// Dump a textual representation of a block to the
// the dump file.

void
Ast_dump_context::dump_block(Block* b)
{
  Ast_dump_traverse_blocks_and_functions adtbf(this);
  b->traverse(&adtbf);
}

// Dump a textual representation of an expression to the
// the dump file.

void
Ast_dump_context::dump_expression(const Expression* e)
{
  e->dump_expression(this);
}

// Dump a textual representation of an expression list to the
// the dump file.

void
Ast_dump_context::dump_expression_list(const Expression_list* el,
				       bool as_pairs /* = false */)
{
  if (el == NULL)
    return;

  for (std::vector<Expression*>::const_iterator it = el->begin();
       it != el->end();
       it++)
    {
      if ( it != el->begin())
        this->ostream() << ",";
      if (*it != NULL)
	(*it)->dump_expression(this);
      else
        this->ostream() << "NULL";
      if (as_pairs)
        {
	  this->ostream() << ":";
	  ++it;
	  (*it)->dump_expression(this);
        }
    }
}

// Dump a textual representation of a typed identifier to the
// the dump file.

void
Ast_dump_context::dump_typed_identifier(const Typed_identifier* ti)
{
  this->ostream() << ti->name() << " ";
  this->dump_type(ti->type());
}

// Dump a textual representation of a typed identifier list to the
// the dump file.

void
Ast_dump_context::dump_typed_identifier_list(
    const Typed_identifier_list* ti_list)
{
  if (ti_list == NULL)
    return;

  for (Typed_identifier_list::const_iterator it = ti_list->begin();
       it != ti_list->end();
       it++)
    {
      if (it != ti_list->begin())
        this->ostream() << ",";
      this->dump_typed_identifier(&(*it));
    }
}

// Dump a textual representation of a temporary variable to the
// the dump file.

void
Ast_dump_context::dump_temp_variable_name(const Statement* s)
{
  go_assert(s->classification() == Statement::STATEMENT_TEMPORARY);
  // Use the statement address as part of the name for the temporary variable.
  this->ostream() << "tmp." << (uintptr_t) s;
}

// Dump a textual representation of a label to the
// the dump file.

void
Ast_dump_context::dump_label_name(const Unnamed_label* l)
{
  // Use the unnamed label address as part of the name for the temporary
  // variable.
  this->ostream() << "label." << (uintptr_t) l;
}

// Produce a textual representation of an operator symbol.

static const char*
op_string(Operator op)
{
// FIXME: This should be in line with symbols that are parsed,
// exported and/or imported.
  switch (op)
    {
    case OPERATOR_PLUS:
      return "+";
    case OPERATOR_MINUS:
      return "-";
    case OPERATOR_NOT:
      return "!";
    case OPERATOR_XOR:
      return "^";
    case OPERATOR_OR:
      return "|";
    case OPERATOR_AND:
      return "&";
    case OPERATOR_MULT:
      return "*";
    case OPERATOR_OROR:
      return "||";
    case OPERATOR_ANDAND:
      return "&&";
    case OPERATOR_EQEQ:
      return "==";
    case OPERATOR_NOTEQ:
      return "!=";
    case OPERATOR_LT:
      return "<";
    case OPERATOR_LE:
      return "<=";
    case OPERATOR_GT:
      return ">";
    case OPERATOR_GE:
      return ">=";
    case OPERATOR_DIV:
      return "/";
    case OPERATOR_MOD:
      return "%";
    case OPERATOR_LSHIFT:
      return "<<";
    case OPERATOR_RSHIFT:
      return "//";
    case OPERATOR_BITCLEAR:
      return "&^";
    case OPERATOR_CHANOP:
      return "<-";
    case OPERATOR_PLUSEQ:
      return "+=";
    case OPERATOR_MINUSEQ:
      return "-=";
    case OPERATOR_OREQ:
      return "|=";
    case OPERATOR_XOREQ:
      return "^=";
    case OPERATOR_MULTEQ:
      return "*=";
    case OPERATOR_DIVEQ:
      return "/=";
    case OPERATOR_MODEQ:
      return "%=";
    case OPERATOR_LSHIFTEQ:
      return "<<=";
    case OPERATOR_RSHIFTEQ:
      return ">>=";
    case OPERATOR_ANDEQ:
      return "&=";
    case OPERATOR_BITCLEAREQ:
      return "&^=";
    case OPERATOR_PLUSPLUS:
      return "++";
    case OPERATOR_MINUSMINUS:
      return "--";
    case OPERATOR_COLON:
      return ":";
    case OPERATOR_COLONEQ:
      return ":=";
    case OPERATOR_SEMICOLON:
      return ";";
    case OPERATOR_DOT:
      return ".";
    case OPERATOR_ELLIPSIS:
      return "...";
    case OPERATOR_COMMA:
      return ",";
    case OPERATOR_LPAREN:
      return "(";
    case OPERATOR_RPAREN:
      return ")";
    case OPERATOR_LCURLY:
      return "{";
    case OPERATOR_RCURLY:
      return "}";
    case OPERATOR_LSQUARE:
      return "[";
    case OPERATOR_RSQUARE:
      return "]";
    default:
      go_unreachable();
    }
  return NULL;
}

// Dump a textual representation of an operator to the
// the dump file.

void
Ast_dump_context::dump_operator(Operator op)
{
  this->ostream() << op_string(op);
}

// Size of a single indent.

const int Ast_dump_context::offset_ = 2;

// Print indenting spaces to dump file.

void
Ast_dump_context::print_indent()
{
  for (int i = 0; i < this->indent_ * this->offset_; i++)
    this->ostream() << " ";
}

// Dump a textual representation of the ast to the
// the dump file.

void Gogo::dump_ast(const char* basename)
{
  if (::ast_dump_flag.is_enabled())
    {
      Ast_dump_context adc;
      adc.dump(this, basename);
    }
}

// Implementation of String_dump interface.

void
Ast_dump_context::write_c_string(const char* s)
{
  this->ostream() << s;
}

void
Ast_dump_context::write_string(const std::string& s)
{
  this->ostream() << s;
}

// Dump statement to stream.

void
Ast_dump_context::dump_to_stream(const Statement* stm, std::ostream* out)
{
  Ast_dump_context adc(out, false);
  stm->dump_statement(&adc);
}

// Dump expression to stream.

void
Ast_dump_context::dump_to_stream(const Expression* expr, std::ostream* out)
{
  Ast_dump_context adc(out, false);
  expr->dump_expression(&adc);
}

// Dump an expression to std::cerr. This is intended to be used
// from within a debugging session.

void
debug_go_expression(const Expression* expr)
{
  if (expr == NULL)
    std::cerr << "<null>";
  else
    {
      Ast_dump_context::dump_to_stream(expr, &std::cerr);
      std::string lstr = Linemap::location_to_string(expr->location());
      std::cerr << " // loc " << lstr << std::endl;
    }
}

// Shallow dump of stmt to std::cerr. This is intended to be used
// from within a debugging session.

void
debug_go_statement(const Statement* stmt)
{
  if (stmt == NULL)
    std::cerr << "<null>\n";
  else
    {
      std::string lstr = Linemap::location_to_string(stmt->location());
      Statement *ncstmt = const_cast<Statement*>(stmt);
      Block_statement* bs = ncstmt->block_statement();
      if (bs != NULL)
        std::cerr << "Block " << bs->block()
                  << " // location: " << lstr << std::endl;
      else
        Ast_dump_context::dump_to_stream(stmt, &std::cerr);
    }
}

// Deep dump of statement to std::cerr. This is intended to be used
// from within a debugging session.

void
debug_go_statement_deep(const Statement* statement)
{
  Ast_dump_context adc(&std::cerr, true);
  statement->dump_statement(&adc);
}

// Shallow dump of a block to std::cerr. This is intended to be used
// from within a debugging session.

void
debug_go_block(const Block* block)
{
  if (block == NULL)
    std::cerr << "<null>";
  else
    {
      std::cerr << "Block " << block
                << " (enclosing " << block->enclosing() << "):\n";
      const std::vector<Statement*>* stmts = block->statements();
      if (stmts != NULL)
        {
          for (size_t i = 0; i < stmts->size(); ++i)
            {
              debug_go_statement(stmts->at(i));
            }
        }
    }
}

// Deep dump of a block to std:cerr. This is intended to be used
// from within a debugging session.

void
debug_go_block_deep(const Block* block)
{
  Ast_dump_context adc(&std::cerr, true);
  Block* ncblock = const_cast<Block*>(block);
  adc.dump_block(ncblock);
}

class Type_dumper
{
  typedef Unordered_map(const Type*, unsigned) idx_map;
 public:
  Type_dumper(const Type* type)
      : top_(type), ntypes_(0)
  {
    this->worklist_.push_back(type);
  }

  void visit();

  std::string stringResult() { return ss_.str(); }

 private:
  void emitpre(unsigned tag, const Type* addr);
  void typeref(const char*, const Type*, const char *);
  void visit_forward_declaration_type(const Forward_declaration_type* fdt);
  void visit_function_type(const Function_type* ft);
  void visit_struct_type(const Struct_type* st);
  void visit_array_type(const Array_type* at);
  void visit_map_type(const Map_type* mt);
  void visit_channel_type(const Channel_type* mt);
  void visit_interface_type(const Interface_type* mt);
  void visit_methods(const Typed_identifier_list* methods,
                     const char *tag);
  std::pair<bool, unsigned> lookup(const Type*);

  static const unsigned notag = 0xffffffff;

 private:
  const Type* top_;
  idx_map types_;
  unsigned ntypes_;
  std::list<const Type*> worklist_;
  std::ostringstream ss_;
};

// Look up a type, installing it in 'types_'. Return is <found, N>
// where 'found' is true if type had been previously recorded, and N
// is the index/tag assigned to N.  The input argument is appended to
// the work list if this is the first time we've seen it.

std::pair<bool, unsigned> Type_dumper::lookup(const Type* t)
{
  std::pair<const Type*, unsigned> entry = std::make_pair(t, this->ntypes_);
  std::pair<idx_map::iterator, bool> ins = this->types_.insert(entry);
  if (ins.second)
    {
      this->ntypes_++;
      if (t != this->top_)
        this->worklist_.push_back(t);
    }
  return std::make_pair(ins.second, ins.first->second);
}

// Emit preamble prior to dumping a type, including the type
// pointer itself and the tag we've assigned it.  If no
// tag is specified (via special "notag" value) and/or the
// pointer is null, then just emit an equivalent amount
// of spaces.

void Type_dumper::emitpre(unsigned tag, const Type* ptr)
{
  char tbuf[50], pbuf[50], buf[200];

  tbuf[0] = '\0';
  if (tag != notag)
    snprintf(tbuf, sizeof tbuf, "T%u", tag);

  pbuf[0] = '\0';
  if (ptr != NULL)
    snprintf(pbuf, sizeof pbuf, "%p", (const void*) ptr);

  snprintf(buf, sizeof buf, "%8s %16s  ", tbuf, pbuf);
  this->ss_ << buf;
}

// Emit a reference to a type into the dump buffer. In most cases this means
// just the type tag, but for named types we also emit the name, and for
// simple/primitive types (ex: int64) we emit the type itself. If "pref" is
// non-NULL, emit the string prior to the reference, and if "suf" is non-NULL,
// emit it following the reference.

void Type_dumper::typeref(const char* pref, const Type* t, const char* suf)
{
  if (pref != NULL)
    this->ss_ << pref;
  std::pair<bool, unsigned> p = this->lookup(t);
  unsigned tag = p.second;
  switch (t->classification())
    {
      case Type::TYPE_NAMED:
        {
          const Named_type* nt = t->named_type();
          const Named_object* no = nt->named_object();
          this->ss_ << "'" << no->message_name() << "' -> ";
          const Type* underlying = nt->real_type();
          this->typeref(NULL, underlying, NULL);
          break;
        }
      case Type::TYPE_POINTER:
        this->typeref("*", t->points_to(), NULL);
        break;
      case Type::TYPE_ERROR:
        this->ss_ << "error_type";
        break;
      case Type::TYPE_INTEGER:
        {
          const Integer_type* it = t->integer_type();
          if (it->is_abstract())
            this->ss_ << "abstract_int";
          else
            this->ss_ << (it->is_unsigned() ? "u" : "") << "int" << it->bits();
          break;
        }
      case Type::TYPE_FLOAT:
        {
          const Float_type* ft = t->float_type();
          if (ft->is_abstract())
            this->ss_ << "abstract_float";
          else
            this->ss_ << "float" << ft->bits();
          break;
        }
      case Type::TYPE_COMPLEX:
        {
          const Complex_type* ct = t->complex_type();
          if (ct->is_abstract())
            this->ss_ << "abstract_complex";
          else
            this->ss_ << "complex" << ct->bits();
          break;
        }
      case Type::TYPE_BOOLEAN:
        this->ss_ << "bool";
        break;
      case Type::TYPE_STRING:
        this->ss_ << "string";
        break;
      case Type::TYPE_NIL:
        this->ss_ << "nil_type";
        break;
    case Type::TYPE_VOID:
        this->ss_ << "void_type";
        break;
    case Type::TYPE_FUNCTION:
    case Type::TYPE_STRUCT:
    case Type::TYPE_ARRAY:
    case Type::TYPE_MAP:
    case Type::TYPE_CHANNEL:
    case Type::TYPE_FORWARD:
    case Type::TYPE_INTERFACE:
      this->ss_ << "T" << tag;
      break;

    default:
      // This is a debugging routine, so instead of a go_unreachable()
      // issue a warning/error, to allow for the possibility that the
      // compiler we're debugging is in a bad state.
      this->ss_ << "<??? " << ((unsigned)t->classification()) << "> "
                << "T" << tag;
    }
  if (suf != NULL)
    this->ss_ << suf;
}

void Type_dumper::visit_forward_declaration_type(const Forward_declaration_type* fdt)
{
  this->ss_ << "forward_declaration_type ";
  if (fdt->is_defined())
    this->typeref("-> ", fdt->real_type(), NULL);
  else
    this->ss_ << "'" << fdt->name() << "'";
  this->ss_ << "\n";
}

void Type_dumper::visit_function_type(const Function_type* ft)
{
  this->ss_ << "function\n";
  const Typed_identifier* rec = ft->receiver();
  if (rec != NULL)
    {
      this->emitpre(notag, NULL);
      this->typeref("receiver ", rec->type(), "\n");
    }
  const Typed_identifier_list* parameters = ft->parameters();
  if (parameters != NULL)
    {
      for (Typed_identifier_list::const_iterator p = parameters->begin();
	   p != parameters->end();
	   ++p)
        {
          this->emitpre(notag, NULL);
          this->typeref(" param ", p->type(), "\n");
        }
    }
  const Typed_identifier_list* results = ft->results();
  if (results != NULL)
    {
      for (Typed_identifier_list::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
        {
          this->emitpre(notag, NULL);
          this->typeref(" result ", p->type(), "\n");
        }
    }
}

void Type_dumper::visit_struct_type(const Struct_type* st)
{
  this->ss_ << "struct\n";
  const Struct_field_list* fields = st->fields();
  if (fields != NULL)
    {
      for (Struct_field_list::const_iterator p = fields->begin();
           p != fields->end();
           ++p)
        {
          this->emitpre(notag, NULL);
          this->typeref(" field ", p->type(), "\n");
        }
    }
}

void Type_dumper::visit_array_type(const Array_type* at)
{
  this->ss_ << "array [";
  if (at->length() != NULL)
    {
      int64_t len = 0;
      if (at->int_length(&len))
        this->ss_ << len;
    }
  this->typeref("] ", at->element_type(), "\n");
}

void Type_dumper::visit_map_type(const Map_type* mt)
{
  this->ss_ << "map [";
  this->typeref(NULL, mt->key_type(), NULL);
  this->typeref("] ", mt->val_type(), "\n");
}

void Type_dumper::visit_methods(const Typed_identifier_list* methods,
                                const char *tag)
{
  if (tag != NULL)
    {
      this->emitpre(notag, NULL);
      this->ss_ << tag << "\n";
    }
  for (Typed_identifier_list::const_iterator p = methods->begin();
       p != methods->end();
       ++p)
    {
      this->emitpre(notag, NULL);
      if (p->name().empty())
        this->typeref("  embedded method ", p->type(), "\n");
      else
        {
          this->ss_ << "  method '" << p->name() << "' ";
          this->typeref(NULL, p->type(), "\n");
        }
    }
}

void Type_dumper::visit_interface_type(const Interface_type* it)
{
  const Typed_identifier_list* methods =
      (it->methods_are_finalized() ? it->methods() : it->local_methods());
  if (methods == NULL)
    {
      this->ss_ << "empty_interface\n";
      return;
    }
  this->ss_ << "interface";
  if (! it->methods_are_finalized())
    {
      this->ss_ << " [unfinalized]\n";
      visit_methods(it->local_methods(), NULL);
    }
  else
    {
      this->ss_ << "\n";
      visit_methods(it->local_methods(), "[parse_methods]");
      visit_methods(it->methods(), "[all_methods]");
    }
}

void Type_dumper::visit_channel_type(const Channel_type* ct)
{
  this->ss_ << "channel {";
  if (ct->may_send())
    this->ss_ << " send";
  if (ct->may_receive())
    this->ss_ << " receive";
  this->typeref(" } ", ct->element_type(), "\n");
}

void Type_dumper::visit()
{
  while (! this->worklist_.empty()) {
    const Type* t = this->worklist_.front();
    this->worklist_.pop_front();

    std::pair<bool, unsigned> p = this->lookup(t);
    unsigned tag = p.second;
    this->emitpre(tag, t);

    switch(t->classification())
      {
        case Type::TYPE_ERROR:
        case Type::TYPE_INTEGER:
        case Type::TYPE_FLOAT:
        case Type::TYPE_COMPLEX:
        case Type::TYPE_BOOLEAN:
        case Type::TYPE_STRING:
        case Type::TYPE_VOID:
        case Type::TYPE_POINTER:
        case Type::TYPE_NIL:
        case Type::TYPE_NAMED:
          this->typeref(NULL, t, "\n");
          break;
        case Type::TYPE_FORWARD:
          this->visit_forward_declaration_type(t->forward_declaration_type());
          break;

        case Type::TYPE_FUNCTION:
          this->visit_function_type(t->function_type());
          break;
        case Type::TYPE_STRUCT:
          this->visit_struct_type(t->struct_type());
          break;
        case Type::TYPE_ARRAY:
          this->visit_array_type(t->array_type());
          break;
        case Type::TYPE_MAP:
          this->visit_map_type(t->map_type());
          break;
        case Type::TYPE_CHANNEL:
          this->visit_channel_type(t->channel_type());
          break;
        case Type::TYPE_INTERFACE:
          this->visit_interface_type(t->interface_type());
          break;
        default:
          // This is a debugging routine, so instead of a go_unreachable()
          // issue a warning/error, to allow for the possibility that the
          // compiler we're debugging is in a bad state.
          this->ss_ << "<unknown/unrecognized classification "
                    << ((unsigned)t->classification()) << ">\n";
      }
  }
}

// Dump a Go type for debugging purposes. This is a deep as opposed
// to shallow dump; all of the types reachable from the specified
// type will be dumped in addition to the type itself.

void debug_go_type(const Type* type)
{
  if (type == NULL)
    {
      std::cerr << "<NULL type>\n";
      return;
    }
  Type_dumper dumper(type);
  dumper.visit();
  std::cerr << dumper.stringResult();
}

void debug_go_type(Type* type)
{
  const Type* ctype = type;
  debug_go_type(ctype);
}
