// parse.cc -- Go frontend parser.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "lex.h"
#include "gogo.h"
#include "go-diagnostics.h"
#include "types.h"
#include "statements.h"
#include "expressions.h"
#include "parse.h"

// Struct Parse::Enclosing_var_comparison.

// Return true if v1 should be considered to be less than v2.

bool
Parse::Enclosing_var_comparison::operator()(const Enclosing_var& v1,
					    const Enclosing_var& v2) const
{
  if (v1.var() == v2.var())
    return false;

  const std::string& n1(v1.var()->name());
  const std::string& n2(v2.var()->name());
  int i = n1.compare(n2);
  if (i < 0)
    return true;
  else if (i > 0)
    return false;

  // If we get here it means that a single nested function refers to
  // two different variables defined in enclosing functions, and both
  // variables have the same name.  I think this is impossible.
  go_unreachable();
}

// Class Parse.

Parse::Parse(Lex* lex, Gogo* gogo)
  : lex_(lex),
    token_(Token::make_invalid_token(Linemap::unknown_location())),
    unget_token_(Token::make_invalid_token(Linemap::unknown_location())),
    unget_token_valid_(false),
    is_erroneous_function_(false),
    gogo_(gogo),
    break_stack_(NULL),
    continue_stack_(NULL),
    enclosing_vars_()
{
}

// Return the current token.

const Token*
Parse::peek_token()
{
  if (this->unget_token_valid_)
    return &this->unget_token_;
  if (this->token_.is_invalid())
    this->token_ = this->lex_->next_token();
  return &this->token_;
}

// Advance to the next token and return it.

const Token*
Parse::advance_token()
{
  if (this->unget_token_valid_)
    {
      this->unget_token_valid_ = false;
      if (!this->token_.is_invalid())
	return &this->token_;
    }
  this->token_ = this->lex_->next_token();
  return &this->token_;
}

// Push a token back on the input stream.

void
Parse::unget_token(const Token& token)
{
  go_assert(!this->unget_token_valid_);
  this->unget_token_ = token;
  this->unget_token_valid_ = true;
}

// The location of the current token.

Location
Parse::location()
{
  return this->peek_token()->location();
}

// IdentifierList = identifier { "," identifier } .

void
Parse::identifier_list(Typed_identifier_list* til)
{
  const Token* token = this->peek_token();
  while (true)
    {
      if (!token->is_identifier())
	{
	  go_error_at(this->location(), "expected identifier");
	  return;
	}
      std::string name =
	this->gogo_->pack_hidden_name(token->identifier(),
				      token->is_identifier_exported());
      til->push_back(Typed_identifier(name, NULL, token->location()));
      token = this->advance_token();
      if (!token->is_op(OPERATOR_COMMA))
	return;
      token = this->advance_token();
    }
}

// ExpressionList = Expression { "," Expression } .

// If MAY_BE_COMPOSITE_LIT is true, an expression may be a composite
// literal.

// If MAY_BE_SINK is true, the expressions in the list may be "_".

Expression_list*
Parse::expression_list(Expression* first, bool may_be_sink,
		       bool may_be_composite_lit)
{
  Expression_list* ret = new Expression_list();
  if (first != NULL)
    ret->push_back(first);
  while (true)
    {
      ret->push_back(this->expression(PRECEDENCE_NORMAL, may_be_sink,
				      may_be_composite_lit, NULL, NULL));

      const Token* token = this->peek_token();
      if (!token->is_op(OPERATOR_COMMA))
	return ret;

      // Most expression lists permit a trailing comma.
      Location location = token->location();
      this->advance_token();
      if (!this->expression_may_start_here())
	{
	  this->unget_token(Token::make_operator_token(OPERATOR_COMMA,
						       location));
	  return ret;
	}
    }
}

// QualifiedIdent = [ PackageName "." ] identifier .
// PackageName = identifier .

// This sets *PNAME to the identifier and sets *PPACKAGE to the
// package or NULL if there isn't one.  This returns true on success,
// false on failure in which case it will have emitted an error
// message.

bool
Parse::qualified_ident(std::string* pname, Named_object** ppackage)
{
  const Token* token = this->peek_token();
  if (!token->is_identifier())
    {
      go_error_at(this->location(), "expected identifier");
      return false;
    }

  std::string name = token->identifier();
  bool is_exported = token->is_identifier_exported();
  name = this->gogo_->pack_hidden_name(name, is_exported);

  token = this->advance_token();
  if (!token->is_op(OPERATOR_DOT))
    {
      *pname = name;
      *ppackage = NULL;
      return true;
    }

  Named_object* package = this->gogo_->lookup(name, NULL);
  if (package == NULL || !package->is_package())
    {
      if (package == NULL)
	go_error_at(this->location(), "reference to undefined name %qs",
		    Gogo::message_name(name).c_str());
      else
	go_error_at(this->location(), "expected package");
      // We expect . IDENTIFIER; skip both.
      if (this->advance_token()->is_identifier())
	this->advance_token();
      return false;
    }

  package->package_value()->note_usage(Gogo::unpack_hidden_name(name));

  token = this->advance_token();
  if (!token->is_identifier())
    {
      go_error_at(this->location(), "expected identifier");
      return false;
    }

  name = token->identifier();

  if (name == "_")
    {
      go_error_at(this->location(), "invalid use of %<_%>");
      name = Gogo::erroneous_name();
    }

  if (package->name() == this->gogo_->package_name())
    name = this->gogo_->pack_hidden_name(name,
					 token->is_identifier_exported());

  *pname = name;
  *ppackage = package;

  this->advance_token();

  return true;
}

// Type = TypeName | TypeLit | "(" Type ")" .
// TypeLit =
// 	ArrayType | StructType | PointerType | FunctionType | InterfaceType |
// 	SliceType | MapType | ChannelType .

Type*
Parse::type()
{
  const Token* token = this->peek_token();
  if (token->is_identifier())
    return this->type_name(true);
  else if (token->is_op(OPERATOR_LSQUARE))
    return this->array_type(false);
  else if (token->is_keyword(KEYWORD_CHAN)
	   || token->is_op(OPERATOR_CHANOP))
    return this->channel_type();
  else if (token->is_keyword(KEYWORD_INTERFACE))
    return this->interface_type(true);
  else if (token->is_keyword(KEYWORD_FUNC))
    {
      Location location = token->location();
      this->advance_token();
      Type* type = this->signature(NULL, location);
      if (type == NULL)
	return Type::make_error_type();
      return type;
    }
  else if (token->is_keyword(KEYWORD_MAP))
    return this->map_type();
  else if (token->is_keyword(KEYWORD_STRUCT))
    return this->struct_type();
  else if (token->is_op(OPERATOR_MULT))
    return this->pointer_type();
  else if (token->is_op(OPERATOR_LPAREN))
    {
      this->advance_token();
      Type* ret = this->type();
      if (this->peek_token()->is_op(OPERATOR_RPAREN))
	this->advance_token();
      else
	{
	  if (!ret->is_error_type())
	    go_error_at(this->location(), "expected %<)%>");
	}
      return ret;
    }
  else
    {
      go_error_at(token->location(), "expected type");
      return Type::make_error_type();
    }
}

bool
Parse::type_may_start_here()
{
  const Token* token = this->peek_token();
  return (token->is_identifier()
	  || token->is_op(OPERATOR_LSQUARE)
	  || token->is_op(OPERATOR_CHANOP)
	  || token->is_keyword(KEYWORD_CHAN)
	  || token->is_keyword(KEYWORD_INTERFACE)
	  || token->is_keyword(KEYWORD_FUNC)
	  || token->is_keyword(KEYWORD_MAP)
	  || token->is_keyword(KEYWORD_STRUCT)
	  || token->is_op(OPERATOR_MULT)
	  || token->is_op(OPERATOR_LPAREN));
}

// TypeName = QualifiedIdent .

// If MAY_BE_NIL is true, then an identifier with the value of the
// predefined constant nil is accepted, returning the nil type.

Type*
Parse::type_name(bool issue_error)
{
  Location location = this->location();

  std::string name;
  Named_object* package;
  if (!this->qualified_ident(&name, &package))
    return Type::make_error_type();

  Named_object* named_object;
  if (package == NULL)
    named_object = this->gogo_->lookup(name, NULL);
  else
    {
      named_object = package->package_value()->lookup(name);
      if (named_object == NULL
	  && issue_error
	  && package->name() != this->gogo_->package_name())
	{
	  // Check whether the name is there but hidden.
	  std::string s = ('.' + package->package_value()->pkgpath()
			   + '.' + name);
	  named_object = package->package_value()->lookup(s);
	  if (named_object != NULL)
	    {
	      Package* p = package->package_value();
	      const std::string& packname(p->package_name());
	      go_error_at(location,
			  "invalid reference to hidden type %<%s.%s%>",
			  Gogo::message_name(packname).c_str(),
			  Gogo::message_name(name).c_str());
	      issue_error = false;
	    }
	}
    }

  bool ok = true;
  if (named_object == NULL)
    {
      if (package == NULL)
	named_object = this->gogo_->add_unknown_name(name, location);
      else
	{
	  const std::string& packname(package->package_value()->package_name());
	  go_error_at(location, "reference to undefined identifier %<%s.%s%>",
		      Gogo::message_name(packname).c_str(),
		      Gogo::message_name(name).c_str());
	  issue_error = false;
	  ok = false;
	}
    }
  else if (named_object->is_type())
    {
      if (!named_object->type_value()->is_visible())
	ok = false;
    }
  else if (named_object->is_unknown() || named_object->is_type_declaration())
    ;
  else
    ok = false;

  if (!ok)
    {
      if (issue_error)
	go_error_at(location, "expected type");
      return Type::make_error_type();
    }

  if (named_object->is_type())
    return named_object->type_value();
  else if (named_object->is_unknown() || named_object->is_type_declaration())
    return Type::make_forward_declaration(named_object);
  else
    go_unreachable();
}

// ArrayType = "[" [ ArrayLength ] "]" ElementType .
// ArrayLength = Expression .
// ElementType = CompleteType .

Type*
Parse::array_type(bool may_use_ellipsis)
{
  go_assert(this->peek_token()->is_op(OPERATOR_LSQUARE));
  const Token* token = this->advance_token();

  Expression* length = NULL;
  if (token->is_op(OPERATOR_RSQUARE))
    this->advance_token();
  else
    {
      if (!token->is_op(OPERATOR_ELLIPSIS))
	length = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
      else if (may_use_ellipsis)
	{
	  // An ellipsis is used in composite literals to represent a
	  // fixed array of the size of the number of elements.  We
	  // use a length of nil to represent this, and change the
	  // length when parsing the composite literal.
	  length = Expression::make_nil(this->location());
	  this->advance_token();
	}
      else
	{
	  go_error_at(this->location(),
		      "use of %<[...]%> outside of array literal");
	  length = Expression::make_error(this->location());
	  this->advance_token();
	}
      if (!this->peek_token()->is_op(OPERATOR_RSQUARE))
	{
	  go_error_at(this->location(), "expected %<]%>");
	  return Type::make_error_type();
	}
      this->advance_token();
    }

  Type* element_type = this->type();
  if (element_type->is_error_type())
    return Type::make_error_type();

  return Type::make_array_type(element_type, length);
}

// MapType = "map" "[" KeyType "]" ValueType .
// KeyType = CompleteType .
// ValueType = CompleteType .

Type*
Parse::map_type()
{
  Location location = this->location();
  go_assert(this->peek_token()->is_keyword(KEYWORD_MAP));
  if (!this->advance_token()->is_op(OPERATOR_LSQUARE))
    {
      go_error_at(this->location(), "expected %<[%>");
      return Type::make_error_type();
    }
  this->advance_token();

  Type* key_type = this->type();

  if (!this->peek_token()->is_op(OPERATOR_RSQUARE))
    {
      go_error_at(this->location(), "expected %<]%>");
      return Type::make_error_type();
    }
  this->advance_token();

  Type* value_type = this->type();

  if (key_type->is_error_type() || value_type->is_error_type())
    return Type::make_error_type();

  return Type::make_map_type(key_type, value_type, location);
}

// StructType     = "struct" "{" { FieldDecl ";" } "}" .

Type*
Parse::struct_type()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_STRUCT));
  Location location = this->location();
  if (!this->advance_token()->is_op(OPERATOR_LCURLY))
    {
      Location token_loc = this->location();
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON)
	  && this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(token_loc, "unexpected semicolon or newline before %<{%>");
      else
	{
	  go_error_at(this->location(), "expected %<{%>");
	  return Type::make_error_type();
	}
    }
  this->advance_token();

  Struct_field_list* sfl = new Struct_field_list;
  while (!this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      this->field_decl(sfl);
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	this->advance_token();
      else if (!this->peek_token()->is_op(OPERATOR_RCURLY))
	{
	  go_error_at(this->location(), "expected %<;%> or %<}%> or newline");
	  if (!this->skip_past_error(OPERATOR_RCURLY))
	    return Type::make_error_type();
	}
    }
  this->advance_token();

  for (Struct_field_list::const_iterator pi = sfl->begin();
       pi != sfl->end();
       ++pi)
    {
      if (pi->type()->is_error_type())
	return pi->type();
      for (Struct_field_list::const_iterator pj = pi + 1;
	   pj != sfl->end();
	   ++pj)
	{
	  if (pi->field_name() == pj->field_name()
	      && !Gogo::is_sink_name(pi->field_name()))
	    go_error_at(pi->location(), "duplicate field name %<%s%>",
			Gogo::message_name(pi->field_name()).c_str());
	}
    }

  return Type::make_struct_type(sfl, location);
}

// FieldDecl = (IdentifierList CompleteType | TypeName) [ Tag ] .
// Tag = string_lit .

void
Parse::field_decl(Struct_field_list* sfl)
{
  const Token* token = this->peek_token();
  Location location = token->location();
  bool is_anonymous;
  bool is_anonymous_pointer;
  if (token->is_op(OPERATOR_MULT))
    {
      is_anonymous = true;
      is_anonymous_pointer = true;
    }
  else if (token->is_identifier())
    {
      std::string id = token->identifier();
      bool is_id_exported = token->is_identifier_exported();
      Location id_location = token->location();
      token = this->advance_token();
      is_anonymous = (token->is_op(OPERATOR_SEMICOLON)
		      || token->is_op(OPERATOR_RCURLY)
		      || token->is_op(OPERATOR_DOT)
		      || token->is_string());
      is_anonymous_pointer = false;
      this->unget_token(Token::make_identifier_token(id, is_id_exported,
						     id_location));
    }
  else
    {
      go_error_at(this->location(), "expected field name");
      this->gogo_->mark_locals_used();
      while (!token->is_op(OPERATOR_SEMICOLON)
	     && !token->is_op(OPERATOR_RCURLY)
	     && !token->is_eof())
	token = this->advance_token();
      return;
    }

  if (is_anonymous)
    {
      if (is_anonymous_pointer)
	{
	  this->advance_token();
	  if (!this->peek_token()->is_identifier())
	    {
	      go_error_at(this->location(), "expected field name");
	      this->gogo_->mark_locals_used();
	      while (!token->is_op(OPERATOR_SEMICOLON)
		     && !token->is_op(OPERATOR_RCURLY)
		     && !token->is_eof())
		token = this->advance_token();
	      return;
	    }
	}
      Type* type = this->type_name(true);

      std::string tag;
      if (this->peek_token()->is_string())
	{
	  tag = this->peek_token()->string_value();
	  this->advance_token();
	}

      if (!type->is_error_type())
	{
	  if (is_anonymous_pointer)
	    type = Type::make_pointer_type(type);
	  sfl->push_back(Struct_field(Typed_identifier("", type, location)));
	  if (!tag.empty())
	    sfl->back().set_tag(tag);
	}
    }
  else
    {
      Typed_identifier_list til;
      while (true)
	{
	  token = this->peek_token();
	  if (!token->is_identifier())
	    {
	      go_error_at(this->location(), "expected identifier");
	      return;
	    }
	  std::string name =
	    this->gogo_->pack_hidden_name(token->identifier(),
					  token->is_identifier_exported());
	  til.push_back(Typed_identifier(name, NULL, token->location()));
	  if (!this->advance_token()->is_op(OPERATOR_COMMA))
	    break;
	  this->advance_token();
	}

      Type* type = this->type();

      std::string tag;
      if (this->peek_token()->is_string())
	{
	  tag = this->peek_token()->string_value();
	  this->advance_token();
	}

      for (Typed_identifier_list::iterator p = til.begin();
	   p != til.end();
	   ++p)
	{
	  p->set_type(type);
	  sfl->push_back(Struct_field(*p));
	  if (!tag.empty())
	    sfl->back().set_tag(tag);
	}
    }
}

// PointerType = "*" Type .

Type*
Parse::pointer_type()
{
  go_assert(this->peek_token()->is_op(OPERATOR_MULT));
  this->advance_token();
  Type* type = this->type();
  if (type->is_error_type())
    return type;
  return Type::make_pointer_type(type);
}

// ChannelType   = Channel | SendChannel | RecvChannel .
// Channel       = "chan" ElementType .
// SendChannel   = "chan" "<-" ElementType .
// RecvChannel   = "<-" "chan" ElementType .

Type*
Parse::channel_type()
{
  const Token* token = this->peek_token();
  bool send = true;
  bool receive = true;
  if (token->is_op(OPERATOR_CHANOP))
    {
      if (!this->advance_token()->is_keyword(KEYWORD_CHAN))
	{
	  go_error_at(this->location(), "expected %<chan%>");
	  return Type::make_error_type();
	}
      send = false;
      this->advance_token();
    }
  else
    {
      go_assert(token->is_keyword(KEYWORD_CHAN));
      if (this->advance_token()->is_op(OPERATOR_CHANOP))
	{
	  receive = false;
	  this->advance_token();
	}
    }

  // Better error messages for the common error of omitting the
  // channel element type.
  if (!this->type_may_start_here())
    {
      token = this->peek_token();
      if (token->is_op(OPERATOR_RCURLY))
	go_error_at(this->location(), "unexpected %<}%> in channel type");
      else if (token->is_op(OPERATOR_RPAREN))
	go_error_at(this->location(), "unexpected %<)%> in channel type");
      else if (token->is_op(OPERATOR_COMMA))
	go_error_at(this->location(), "unexpected comma in channel type");
      else
	go_error_at(this->location(), "expected channel element type");
      return Type::make_error_type();
    }

  Type* element_type = this->type();
  return Type::make_channel_type(send, receive, element_type);
}

// Give an error for a duplicate parameter or receiver name.

void
Parse::check_signature_names(const Typed_identifier_list* params,
			     Parse::Names* names)
{
  for (Typed_identifier_list::const_iterator p = params->begin();
       p != params->end();
       ++p)
    {
      if (p->name().empty() || Gogo::is_sink_name(p->name()))
	continue;
      std::pair<std::string, const Typed_identifier*> val =
	std::make_pair(p->name(), &*p);
      std::pair<Parse::Names::iterator, bool> ins = names->insert(val);
      if (!ins.second)
	{
	  go_error_at(p->location(), "redefinition of %qs",
		      Gogo::message_name(p->name()).c_str());
	  go_inform(ins.first->second->location(),
		    "previous definition of %qs was here",
		    Gogo::message_name(p->name()).c_str());
	}
    }
}

// Signature      = Parameters [ Result ] .

// RECEIVER is the receiver if there is one, or NULL.  LOCATION is the
// location of the start of the type.

// This returns NULL on a parse error.

Function_type*
Parse::signature(Typed_identifier* receiver, Location location)
{
  bool is_varargs = false;
  Typed_identifier_list* params;
  bool params_ok = this->parameters(&params, &is_varargs);

  Typed_identifier_list* results = NULL;
  if (this->peek_token()->is_op(OPERATOR_LPAREN)
      || this->type_may_start_here())
    {
      if (!this->result(&results))
	return NULL;
    }

  if (!params_ok)
    return NULL;

  Parse::Names names;
  if (receiver != NULL)
    names[receiver->name()] = receiver;
  if (params != NULL)
    this->check_signature_names(params, &names);
  if (results != NULL)
    this->check_signature_names(results, &names);

  Function_type* ret = Type::make_function_type(receiver, params, results,
						location);
  if (is_varargs)
    ret->set_is_varargs();
  return ret;
}

// Parameters     = "(" [ ParameterList [ "," ] ] ")" .

// This returns false on a parse error.

bool
Parse::parameters(Typed_identifier_list** pparams, bool* is_varargs)
{
  *pparams = NULL;

  if (!this->peek_token()->is_op(OPERATOR_LPAREN))
    {
      go_error_at(this->location(), "expected %<(%>");
      return false;
    }

  Typed_identifier_list* params = NULL;
  bool saw_error = false;

  const Token* token = this->advance_token();
  if (!token->is_op(OPERATOR_RPAREN))
    {
      params = this->parameter_list(is_varargs);
      if (params == NULL)
	saw_error = true;
      token = this->peek_token();
    }

  // The optional trailing comma is picked up in parameter_list.

  if (!token->is_op(OPERATOR_RPAREN))
    {
      go_error_at(this->location(), "expected %<)%>");
      return false;
    }
  this->advance_token();

  if (saw_error)
    return false;

  *pparams = params;
  return true;
}

// ParameterList  = ParameterDecl { "," ParameterDecl } .

// This sets *IS_VARARGS if the list ends with an ellipsis.
// IS_VARARGS will be NULL if varargs are not permitted.

// We pick up an optional trailing comma.

// This returns NULL if some error is seen.

Typed_identifier_list*
Parse::parameter_list(bool* is_varargs)
{
  Location location = this->location();
  Typed_identifier_list* ret = new Typed_identifier_list();

  bool saw_error = false;

  // If we see an identifier and then a comma, then we don't know
  // whether we are looking at a list of identifiers followed by a
  // type, or a list of types given by name.  We have to do an
  // arbitrary lookahead to figure it out.

  bool parameters_have_names;
  const Token* token = this->peek_token();
  if (!token->is_identifier())
    {
      // This must be a type which starts with something like '*'.
      parameters_have_names = false;
    }
  else
    {
      std::string name = token->identifier();
      bool is_exported = token->is_identifier_exported();
      Location id_location = token->location();
      token = this->advance_token();
      if (!token->is_op(OPERATOR_COMMA))
	{
	  if (token->is_op(OPERATOR_DOT))
	    {
	      // This is a qualified identifier, which must turn out
	      // to be a type.
	      parameters_have_names = false;
	    }
	  else if (token->is_op(OPERATOR_RPAREN))
	    {
	      // A single identifier followed by a parenthesis must be
	      // a type name.
	      parameters_have_names = false;
	    }
	  else
	    {
	      // An identifier followed by something other than a
	      // comma or a dot or a right parenthesis must be a
	      // parameter name followed by a type.
	      parameters_have_names = true;
	    }

	  this->unget_token(Token::make_identifier_token(name, is_exported,
							 id_location));
	}
      else
	{
	  // An identifier followed by a comma may be the first in a
	  // list of parameter names followed by a type, or it may be
	  // the first in a list of types without parameter names.  To
	  // find out we gather as many identifiers separated by
	  // commas as we can.
	  std::string id_name = this->gogo_->pack_hidden_name(name,
							      is_exported);
	  ret->push_back(Typed_identifier(id_name, NULL, id_location));
	  bool just_saw_comma = true;
	  while (this->advance_token()->is_identifier())
	    {
	      name = this->peek_token()->identifier();
	      is_exported = this->peek_token()->is_identifier_exported();
	      id_location = this->peek_token()->location();
	      id_name = this->gogo_->pack_hidden_name(name, is_exported);
	      ret->push_back(Typed_identifier(id_name, NULL, id_location));
	      if (!this->advance_token()->is_op(OPERATOR_COMMA))
		{
		  just_saw_comma = false;
		  break;
		}
	    }

	  if (just_saw_comma)
	    {
	      // We saw ID1 "," ID2 "," followed by something which
	      // was not an identifier.  We must be seeing the start
	      // of a type, and ID1 and ID2 must be types, and the
	      // parameters don't have names.
	      parameters_have_names = false;
	    }
	  else if (this->peek_token()->is_op(OPERATOR_RPAREN))
	    {
	      // We saw ID1 "," ID2 ")".  ID1 and ID2 must be types,
	      // and the parameters don't have names.
	      parameters_have_names = false;
	    }
	  else if (this->peek_token()->is_op(OPERATOR_DOT))
	    {
	      // We saw ID1 "," ID2 ".".  ID2 must be a package name,
	      // ID1 must be a type, and the parameters don't have
	      // names.
	      parameters_have_names = false;
	      this->unget_token(Token::make_identifier_token(name, is_exported,
							     id_location));
	      ret->pop_back();
	      just_saw_comma = true;
	    }
	  else
	    {
	      // We saw ID1 "," ID2 followed by something other than
	      // ",", ".", or ")".  We must be looking at the start of
	      // a type, and ID1 and ID2 must be parameter names.
	      parameters_have_names = true;
	    }

	  if (parameters_have_names)
	    {
	      go_assert(!just_saw_comma);
	      // We have just seen ID1, ID2 xxx.
	      Type* type;
	      if (!this->peek_token()->is_op(OPERATOR_ELLIPSIS))
		type = this->type();
	      else
		{
		  go_error_at(this->location(),
			      "%<...%> only permits one name");
		  saw_error = true;
		  this->advance_token();
		  type = this->type();
		}
	      for (size_t i = 0; i < ret->size(); ++i)
		ret->set_type(i, type);
	      if (!this->peek_token()->is_op(OPERATOR_COMMA))
		return saw_error ? NULL : ret;
	      if (this->advance_token()->is_op(OPERATOR_RPAREN))
		return saw_error ? NULL : ret;
	    }
	  else
	    {
	      Typed_identifier_list* tret = new Typed_identifier_list();
	      for (Typed_identifier_list::const_iterator p = ret->begin();
		   p != ret->end();
		   ++p)
		{
		  Named_object* no = this->gogo_->lookup(p->name(), NULL);
		  Type* type;
		  if (no == NULL)
		    no = this->gogo_->add_unknown_name(p->name(),
						       p->location());

		  if (no->is_type())
		    type = no->type_value();
		  else if (no->is_unknown() || no->is_type_declaration())
		    type = Type::make_forward_declaration(no);
		  else
		    {
		      go_error_at(p->location(), "expected %<%s%> to be a type",
				  Gogo::message_name(p->name()).c_str());
		      saw_error = true;
		      type = Type::make_error_type();
		    }
		  tret->push_back(Typed_identifier("", type, p->location()));
		}
	      delete ret;
	      ret = tret;
	      if (!just_saw_comma
		  || this->peek_token()->is_op(OPERATOR_RPAREN))
		return saw_error ? NULL : ret;
	    }
	}
    }

  bool mix_error = false;
  this->parameter_decl(parameters_have_names, ret, is_varargs, &mix_error,
		       &saw_error);
  while (this->peek_token()->is_op(OPERATOR_COMMA))
    {
      if (this->advance_token()->is_op(OPERATOR_RPAREN))
	break;
      if (is_varargs != NULL && *is_varargs)
	{
	  go_error_at(this->location(), "%<...%> must be last parameter");
	  saw_error = true;
	}
      this->parameter_decl(parameters_have_names, ret, is_varargs, &mix_error,
			   &saw_error);
    }
  if (mix_error)
    {
      go_error_at(location, "mixed named and unnamed function parameters");
      saw_error = true;
    }
  if (saw_error)
    {
      delete ret;
      return NULL;
    }
  return ret;
}

// ParameterDecl  = [ IdentifierList ] [ "..." ] Type .

void
Parse::parameter_decl(bool parameters_have_names,
		      Typed_identifier_list* til,
		      bool* is_varargs,
		      bool* mix_error,
		      bool* saw_error)
{
  if (!parameters_have_names)
    {
      Type* type;
      Location location = this->location();
      if (!this->peek_token()->is_identifier())
	{
	  if (!this->peek_token()->is_op(OPERATOR_ELLIPSIS))
	    type = this->type();
	  else
	    {
	      if (is_varargs == NULL)
		go_error_at(this->location(), "invalid use of %<...%>");
	      else
		*is_varargs = true;
	      this->advance_token();
	      if (is_varargs == NULL
		  && this->peek_token()->is_op(OPERATOR_RPAREN))
		type = Type::make_error_type();
	      else
		{
		  Type* element_type = this->type();
		  type = Type::make_array_type(element_type, NULL);
		}
	    }
	}
      else
	{
	  type = this->type_name(false);
	  if (type->is_error_type()
	      || (!this->peek_token()->is_op(OPERATOR_COMMA)
		  && !this->peek_token()->is_op(OPERATOR_RPAREN)))
	    {
	      *mix_error = true;
	      while (!this->peek_token()->is_op(OPERATOR_COMMA)
		     && !this->peek_token()->is_op(OPERATOR_RPAREN)
                     && !this->peek_token()->is_eof())
		this->advance_token();
	    }
	}
      if (!type->is_error_type())
	til->push_back(Typed_identifier("", type, location));
      else
	*saw_error = true;
    }
  else
    {
      size_t orig_count = til->size();
      if (this->peek_token()->is_identifier())
	this->identifier_list(til);
      else
	*mix_error = true;
      size_t new_count = til->size();

      Type* type;
      if (!this->peek_token()->is_op(OPERATOR_ELLIPSIS))
	type = this->type();
      else
	{
	  if (is_varargs == NULL)
	    {
	      go_error_at(this->location(), "invalid use of %<...%>");
	      *saw_error = true;
	    }
	  else if (new_count > orig_count + 1)
	    {
	      go_error_at(this->location(), "%<...%> only permits one name");
	      *saw_error = true;
	    }
	  else
	    *is_varargs = true;
	  this->advance_token();
	  Type* element_type = this->type();
	  type = Type::make_array_type(element_type, NULL);
	}
      for (size_t i = orig_count; i < new_count; ++i)
	til->set_type(i, type);
    }
}

// Result         = Parameters | Type .

// This returns false on a parse error.

bool
Parse::result(Typed_identifier_list** presults)
{
  if (this->peek_token()->is_op(OPERATOR_LPAREN))
    return this->parameters(presults, NULL);
  else
    {
      Location location = this->location();
      Type* type = this->type();
      if (type->is_error_type())
	{
	  *presults = NULL;
	  return false;
	}
      Typed_identifier_list* til = new Typed_identifier_list();
      til->push_back(Typed_identifier("", type, location));
      *presults = til;
      return true;
    }
}

// Block = "{" [ StatementList ] "}" .

// Returns the location of the closing brace.

Location
Parse::block()
{
  if (!this->peek_token()->is_op(OPERATOR_LCURLY))
    {
      Location loc = this->location();
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON)
	  && this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(loc, "unexpected semicolon or newline before %<{%>");
      else
	{
	  go_error_at(this->location(), "expected %<{%>");
	  return Linemap::unknown_location();
	}
    }

  const Token* token = this->advance_token();

  if (!token->is_op(OPERATOR_RCURLY))
    {
      this->statement_list();
      token = this->peek_token();
      if (!token->is_op(OPERATOR_RCURLY))
	{
	  if (!token->is_eof() || !saw_errors())
	    go_error_at(this->location(), "expected %<}%>");

	  this->gogo_->mark_locals_used();

	  // Skip ahead to the end of the block, in hopes of avoiding
	  // lots of meaningless errors.
	  Location ret = token->location();
	  int nest = 0;
	  while (!token->is_eof())
	    {
	      if (token->is_op(OPERATOR_LCURLY))
		++nest;
	      else if (token->is_op(OPERATOR_RCURLY))
		{
		  --nest;
		  if (nest < 0)
		    {
		      this->advance_token();
		      break;
		    }
		}
	      token = this->advance_token();
	      ret = token->location();
	    }
	  return ret;
	}
    }

  Location ret = token->location();
  this->advance_token();
  return ret;
}

// InterfaceType      = "interface" "{" [ MethodSpecList ] "}" .
// MethodSpecList     = MethodSpec { ";" MethodSpec } [ ";" ] .

Type*
Parse::interface_type(bool record)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_INTERFACE));
  Location location = this->location();

  if (!this->advance_token()->is_op(OPERATOR_LCURLY))
    {
      Location token_loc = this->location();
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON)
	  && this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(token_loc, "unexpected semicolon or newline before %<{%>");
      else
	{
	  go_error_at(this->location(), "expected %<{%>");
	  return Type::make_error_type();
	}
    }
  this->advance_token();

  Typed_identifier_list* methods = new Typed_identifier_list();
  if (!this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      this->method_spec(methods);
      while (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	{
	  if (this->advance_token()->is_op(OPERATOR_RCURLY))
	    break;
	  this->method_spec(methods);
	}
      if (!this->peek_token()->is_op(OPERATOR_RCURLY))
	{
	  go_error_at(this->location(), "expected %<}%>");
	  while (!this->advance_token()->is_op(OPERATOR_RCURLY))
	    {
	      if (this->peek_token()->is_eof())
		return Type::make_error_type();
	    }
	}
    }
  this->advance_token();

  if (methods->empty())
    {
      delete methods;
      methods = NULL;
    }

  Interface_type* ret;
  if (methods == NULL)
    ret = Type::make_empty_interface_type(location);
  else
    ret = Type::make_interface_type(methods, location);
  if (record)
    this->gogo_->record_interface_type(ret);
  return ret;
}

// MethodSpec         = MethodName Signature | InterfaceTypeName .
// MethodName         = identifier .
// InterfaceTypeName  = TypeName .

void
Parse::method_spec(Typed_identifier_list* methods)
{
  const Token* token = this->peek_token();
  if (!token->is_identifier())
    {
      go_error_at(this->location(), "expected identifier");
      return;
    }

  std::string name = token->identifier();
  bool is_exported = token->is_identifier_exported();
  Location location = token->location();

  if (this->advance_token()->is_op(OPERATOR_LPAREN))
    {
      // This is a MethodName.
      if (name == "_")
	go_error_at(this->location(),
                    "methods must have a unique non-blank name");
      name = this->gogo_->pack_hidden_name(name, is_exported);
      Type* type = this->signature(NULL, location);
      if (type == NULL)
	return;
      methods->push_back(Typed_identifier(name, type, location));
    }
  else
    {
      this->unget_token(Token::make_identifier_token(name, is_exported,
						     location));
      Type* type = this->type_name(false);
      if (type->is_error_type()
	  || (!this->peek_token()->is_op(OPERATOR_SEMICOLON)
	      && !this->peek_token()->is_op(OPERATOR_RCURLY)))
	{
	  if (this->peek_token()->is_op(OPERATOR_COMMA))
	    go_error_at(this->location(),
			"name list not allowed in interface type");
	  else
	    go_error_at(location, "expected signature or type name");
	  this->gogo_->mark_locals_used();
	  token = this->peek_token();
	  while (!token->is_eof()
		 && !token->is_op(OPERATOR_SEMICOLON)
		 && !token->is_op(OPERATOR_RCURLY))
	    token = this->advance_token();
	  return;
	}
      // This must be an interface type, but we can't check that now.
      // We check it and pull out the methods in
      // Interface_type::do_verify.
      methods->push_back(Typed_identifier("", type, location));
    }
}

// Declaration = ConstDecl | TypeDecl | VarDecl | FunctionDecl | MethodDecl .

void
Parse::declaration()
{
  const Token* token = this->peek_token();
  if (token->is_keyword(KEYWORD_CONST))
    this->const_decl();
  else if (token->is_keyword(KEYWORD_TYPE))
    this->type_decl();
  else if (token->is_keyword(KEYWORD_VAR))
    this->var_decl();
  else if (token->is_keyword(KEYWORD_FUNC))
    this->function_decl();
  else
    {
      go_error_at(this->location(), "expected declaration");
      this->advance_token();
    }
}

bool
Parse::declaration_may_start_here()
{
  const Token* token = this->peek_token();
  return (token->is_keyword(KEYWORD_CONST)
	  || token->is_keyword(KEYWORD_TYPE)
	  || token->is_keyword(KEYWORD_VAR)
	  || token->is_keyword(KEYWORD_FUNC));
}

// Decl<P> = P | "(" [ List<P> ] ")" .

void
Parse::decl(void (Parse::*pfn)())
{
  if (this->peek_token()->is_eof())
    {
      if (!saw_errors())
	go_error_at(this->location(), "unexpected end of file");
      return;
    }

  if (!this->peek_token()->is_op(OPERATOR_LPAREN))
    (this->*pfn)();
  else
    {
      if (this->lex_->get_and_clear_pragmas() != 0)
	go_error_at(this->location(),
		    "ignoring compiler directive before group");
      if (this->lex_->has_embeds())
	{
	  this->lex_->clear_embeds();
	  go_error_at(this->location(),
		      "ignoring %<//go:embed%> comment before group");
	}
      if (!this->advance_token()->is_op(OPERATOR_RPAREN))
	{
	  this->list(pfn, true);
	  if (!this->peek_token()->is_op(OPERATOR_RPAREN))
	    {
	      go_error_at(this->location(), "missing %<)%>");
	      while (!this->advance_token()->is_op(OPERATOR_RPAREN))
		{
		  if (this->peek_token()->is_eof())
		    return;
		}
	    }
	}
      this->advance_token();
    }
}

// List<P> = P { ";" P } [ ";" ] .

// In order to pick up the trailing semicolon we need to know what
// might follow.  This is either a '}' or a ')'.

void
Parse::list(void (Parse::*pfn)(), bool follow_is_paren)
{
  (this->*pfn)();
  Operator follow = follow_is_paren ? OPERATOR_RPAREN : OPERATOR_RCURLY;
  while (this->peek_token()->is_op(OPERATOR_SEMICOLON)
	 || this->peek_token()->is_op(OPERATOR_COMMA))
    {
      if (this->peek_token()->is_op(OPERATOR_COMMA))
	go_error_at(this->location(), "unexpected comma");
      if (this->advance_token()->is_op(follow))
	break;
      (this->*pfn)();
    }
}

// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .

void
Parse::const_decl()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_CONST));
  this->advance_token();

  int iota = 0;
  Type* last_type = NULL;
  Expression_list* last_expr_list = NULL;

  if (!this->peek_token()->is_op(OPERATOR_LPAREN))
    this->const_spec(iota, &last_type, &last_expr_list);
  else
    {
      this->advance_token();
      while (!this->peek_token()->is_op(OPERATOR_RPAREN))
	{
	  this->const_spec(iota, &last_type, &last_expr_list);
	  ++iota;
	  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	    this->advance_token();
	  else if (!this->peek_token()->is_op(OPERATOR_RPAREN))
	    {
	      go_error_at(this->location(),
			  "expected %<;%> or %<)%> or newline");
	      if (!this->skip_past_error(OPERATOR_RPAREN))
		return;
	    }
	}
      this->advance_token();
    }

  if (last_expr_list != NULL)
    delete last_expr_list;
}

// ConstSpec = IdentifierList [ [ CompleteType ] "=" ExpressionList ] .

void
Parse::const_spec(int iota, Type** last_type, Expression_list** last_expr_list)
{
  this->check_directives();

  Location loc = this->location();
  Typed_identifier_list til;
  this->identifier_list(&til);

  Type* type = NULL;
  if (this->type_may_start_here())
    {
      type = this->type();
      *last_type = NULL;
      *last_expr_list = NULL;
    }

  Expression_list *expr_list;
  if (!this->peek_token()->is_op(OPERATOR_EQ))
    {
      if (*last_expr_list == NULL)
	{
	  go_error_at(this->location(), "expected %<=%>");
	  return;
	}
      type = *last_type;
      expr_list = new Expression_list;
      for (Expression_list::const_iterator p = (*last_expr_list)->begin();
	   p != (*last_expr_list)->end();
	   ++p)
	{
	  Expression* copy = (*p)->copy();
	  copy->set_location(loc);
	  this->update_references(&copy);
	  expr_list->push_back(copy);
	}
    }
  else
    {
      this->advance_token();
      expr_list = this->expression_list(NULL, false, true);
      *last_type = type;
      if (*last_expr_list != NULL)
	delete *last_expr_list;
      *last_expr_list = expr_list;
    }

  Expression_list::const_iterator pe = expr_list->begin();
  for (Typed_identifier_list::iterator pi = til.begin();
       pi != til.end();
       ++pi, ++pe)
    {
      if (pe == expr_list->end())
	{
	  go_error_at(this->location(), "not enough initializers");
	  return;
	}
      if (type != NULL)
	pi->set_type(type);

      if (!Gogo::is_sink_name(pi->name()))
	this->gogo_->add_constant(*pi, *pe, iota);
      else
	{
	  static int count;
	  char buf[30];
	  snprintf(buf, sizeof buf, ".$sinkconst%d", count);
	  ++count;
	  Typed_identifier ti(std::string(buf), type, pi->location());
	  Named_object* no = this->gogo_->add_constant(ti, *pe, iota);
	  no->const_value()->set_is_sink();
	}
    }
  if (pe != expr_list->end())
    go_error_at(this->location(), "too many initializers");

  return;
}

// Update any references to names to refer to the current names,
// for weird cases like
//
// const X = 1
// func F() {
// 	const (
// 		X = X + X
//		Y
// 	)
// }
//
// where the X + X for the first X is the outer X, but the X + X
// copied for Y is the inner X.

class Update_references : public Traverse
{
 public:
  Update_references(Gogo* gogo)
    : Traverse(traverse_expressions),
      gogo_(gogo)
  { }

  int
  expression(Expression**);

 private:
  Gogo* gogo_;
};

int
Update_references::expression(Expression** pexpr)
{
  Named_object* old_no;
  switch ((*pexpr)->classification())
    {
    case Expression::EXPRESSION_CONST_REFERENCE:
      old_no = (*pexpr)->const_expression()->named_object();
      break;
    case Expression::EXPRESSION_VAR_REFERENCE:
      old_no = (*pexpr)->var_expression()->named_object();
      break;
    case Expression::EXPRESSION_ENCLOSED_VAR_REFERENCE:
      old_no = (*pexpr)->enclosed_var_expression()->variable();
      break;
    case Expression::EXPRESSION_FUNC_REFERENCE:
      old_no = (*pexpr)->func_expression()->named_object();
      break;
    case Expression::EXPRESSION_UNKNOWN_REFERENCE:
      old_no = (*pexpr)->unknown_expression()->named_object();
      break;
    default:
      return TRAVERSE_CONTINUE;
    }

  if (old_no->package() != NULL)
    {
      // This is a qualified reference, so it can't have changed in
      // scope.  FIXME: This probably doesn't handle dot imports
      // correctly.
      return TRAVERSE_CONTINUE;
    }

  Named_object* in_function;
  Named_object* new_no = this->gogo_->lookup(old_no->name(), &in_function);
  if (new_no == old_no)
    return TRAVERSE_CONTINUE;

  // The new name must be a constant, since that is all we have
  // introduced into scope.
  if (!new_no->is_const())
    {
      go_assert(saw_errors());
      return TRAVERSE_CONTINUE;
    }

  *pexpr = Expression::make_const_reference(new_no, (*pexpr)->location());

  return TRAVERSE_CONTINUE;
}

void
Parse::update_references(Expression** pexpr)
{
  Update_references ur(this->gogo_);
  ur.expression(pexpr);
  (*pexpr)->traverse_subexpressions(&ur);
}

// TypeDecl = "type" Decl<TypeSpec> .

void
Parse::type_decl()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_TYPE));
  this->advance_token();
  this->decl(&Parse::type_spec);
}

// TypeSpec = identifier ["="] Type .

void
Parse::type_spec()
{
  unsigned int pragmas = this->lex_->get_and_clear_pragmas();
  this->check_directives();

  const Token* token = this->peek_token();
  if (!token->is_identifier())
    {
      go_error_at(this->location(), "expected identifier");
      return;
    }
  std::string name = token->identifier();
  bool is_exported = token->is_identifier_exported();
  Location location = token->location();
  token = this->advance_token();

  bool is_alias = false;
  if (token->is_op(OPERATOR_EQ))
    {
      is_alias = true;
      token = this->advance_token();
    }

  // The scope of the type name starts at the point where the
  // identifier appears in the source code.  We implement this by
  // declaring the type before we read the type definition.
  Named_object* named_type = NULL;
  if (name != "_")
    {
      name = this->gogo_->pack_hidden_name(name, is_exported);
      named_type = this->gogo_->declare_type(name, location);
    }

  Type* type;
  if (name == "_" && token->is_keyword(KEYWORD_INTERFACE))
    {
      // We call Parse::interface_type explicity here because we do not want
      // to record an interface with a blank type name.
      type = this->interface_type(false);
    }
  else if (!token->is_op(OPERATOR_SEMICOLON))
    type = this->type();
  else
    {
      go_error_at(this->location(),
		  "unexpected semicolon or newline in type declaration");
      type = Type::make_error_type();
    }

  if (type->is_error_type())
    {
      this->gogo_->mark_locals_used();
      while (!this->peek_token()->is_op(OPERATOR_SEMICOLON)
	     && !this->peek_token()->is_eof())
	this->advance_token();
    }

  if (name != "_")
    {
      if (named_type->is_type_declaration())
	{
	  Type* ftype = type->forwarded();
	  if (ftype->forward_declaration_type() != NULL
	      && (ftype->forward_declaration_type()->named_object()
		  == named_type))
	    {
	      go_error_at(location, "invalid recursive type");
	      type = Type::make_error_type();
	    }

	  Named_type* nt = Type::make_named_type(named_type, type, location);
	  if (is_alias)
	    nt->set_is_alias();

	  this->gogo_->define_type(named_type, nt);
	  go_assert(named_type->package() == NULL);

	  if ((pragmas & GOPRAGMA_NOTINHEAP) != 0)
	    {
	      nt->set_not_in_heap();
	      pragmas &= ~GOPRAGMA_NOTINHEAP;
	    }
	  if (pragmas != 0)
	    go_warning_at(location, 0,
			  "ignoring magic %<//go:...%> comment before type");
	}
      else
	{
	  // This will probably give a redefinition error.
	  this->gogo_->add_type(name, type, location);
	}
    }
}

// VarDecl = "var" Decl<VarSpec> .

void
Parse::var_decl()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_VAR));
  this->advance_token();
  this->decl(&Parse::var_spec);
}

// VarSpec = IdentifierList
//             ( CompleteType [ "=" ExpressionList ] | "=" ExpressionList ) .

void
Parse::var_spec()
{
  Location loc = this->location();

  std::vector<std::string>* embeds = NULL;
  if (this->lex_->has_embeds())
    {
      if (!this->gogo_->current_file_imported_embed())
	go_error_at(loc, "invalid go:embed: missing import %<embed%>");
      else
	{
	  embeds = new(std::vector<std::string>);
	  this->lex_->get_and_clear_embeds(embeds);
	}
    }

  this->check_directives();

  // Get the variable names.
  Typed_identifier_list til;
  this->identifier_list(&til);

  if (embeds != NULL)
    {
      if (!this->gogo_->in_global_scope())
	{
	  go_error_at(loc, "go:embed only permitted at package scope");
	  embeds = NULL;
	}
      if (til.size() > 1)
	{
	  go_error_at(loc, "go:embed cannot apply to multiple vars");
	  embeds = NULL;
	}
    }

  Location location = this->location();

  Type* type = NULL;
  Expression_list* init = NULL;
  if (!this->peek_token()->is_op(OPERATOR_EQ))
    {
      type = this->type();
      if (type->is_error_type())
	{
	  this->gogo_->mark_locals_used();
	  while (!this->peek_token()->is_op(OPERATOR_EQ)
		 && !this->peek_token()->is_op(OPERATOR_SEMICOLON)
		 && !this->peek_token()->is_eof())
	    this->advance_token();
	}
      if (this->peek_token()->is_op(OPERATOR_EQ))
	{
	  this->advance_token();
	  init = this->expression_list(NULL, false, true);
	}
    }
  else
    {
      this->advance_token();
      init = this->expression_list(NULL, false, true);
    }

  if (embeds != NULL && init != NULL)
    {
      go_error_at(loc, "go:embed cannot apply to var with initializer");
      embeds = NULL;
    }

  this->init_vars(&til, type, init, false, embeds, location);

  if (init != NULL)
    delete init;
}

// Create variables.  TIL is a list of variable names.  If TYPE is not
// NULL, it is the type of all the variables.  If INIT is not NULL, it
// is an initializer list for the variables.

void
Parse::init_vars(const Typed_identifier_list* til, Type* type,
		 Expression_list* init, bool is_coloneq,
		 std::vector<std::string>* embeds, Location location)
{
  // Check for an initialization which can yield multiple values.
  if (init != NULL && init->size() == 1 && til->size() > 1)
    {
      go_assert(embeds == NULL);
      if (this->init_vars_from_call(til, type, *init->begin(), is_coloneq,
				    location))
	return;
      if (this->init_vars_from_map(til, type, *init->begin(), is_coloneq,
				   location))
	return;
      if (this->init_vars_from_receive(til, type, *init->begin(), is_coloneq,
				       location))
	return;
      if (this->init_vars_from_type_guard(til, type, *init->begin(),
					  is_coloneq, location))
	return;
    }

  if (init != NULL && init->size() != til->size())
    {
      if (init->empty() || !init->front()->is_error_expression())
	go_error_at(location, "wrong number of initializations");
      init = NULL;
      if (type == NULL)
	type = Type::make_error_type();
    }

  // Note that INIT was already parsed with the old name bindings, so
  // we don't have to worry that it will accidentally refer to the
  // newly declared variables.  But we do have to worry about a mix of
  // newly declared variables and old variables if the old variables
  // appear in the initializations.

  Expression_list::const_iterator pexpr;
  if (init != NULL)
    pexpr = init->begin();
  bool any_new = false;
  Expression_list* vars = new Expression_list();
  Expression_list* vals = new Expression_list();
  for (Typed_identifier_list::const_iterator p = til->begin();
       p != til->end();
       ++p)
    {
      if (init != NULL)
	go_assert(pexpr != init->end());
      Named_object* no = this->init_var(*p, type,
					init == NULL ? NULL : *pexpr,
					is_coloneq, false, &any_new,
					vars, vals);
      if (embeds != NULL && no->is_variable())
	no->var_value()->set_embeds(embeds);
      if (init != NULL)
	++pexpr;
    }
  if (init != NULL)
    go_assert(pexpr == init->end());
  if (is_coloneq && !any_new)
    go_error_at(location, "variables redeclared but no variable is new");
  this->finish_init_vars(vars, vals, location);
}

// See if we need to initialize a list of variables from a function
// call.  This returns true if we have set up the variables and the
// initialization.

bool
Parse::init_vars_from_call(const Typed_identifier_list* vars, Type* type,
			   Expression* expr, bool is_coloneq,
			   Location location)
{
  Call_expression* call = expr->call_expression();
  if (call == NULL)
    return false;

  // This is a function call.  We can't check here whether it returns
  // the right number of values, but it might.  Declare the variables,
  // and then assign the results of the call to them.

  call->set_expected_result_count(vars->size());

  Named_object* first_var = NULL;
  unsigned int index = 0;
  bool any_new = false;
  Expression_list* ivars = new Expression_list();
  Expression_list* ivals = new Expression_list();
  for (Typed_identifier_list::const_iterator pv = vars->begin();
       pv != vars->end();
       ++pv, ++index)
    {
      Expression* init = Expression::make_call_result(call, index);
      Named_object* no = this->init_var(*pv, type, init, is_coloneq, false,
					&any_new, ivars, ivals);

      if (this->gogo_->in_global_scope() && no->is_variable())
	{
	  if (first_var == NULL)
	    first_var = no;
	  else
	    {
              // If the current object is a redefinition of another object, we
              // might have already recorded the dependency relationship between
              // it and the first variable.  Either way, an error will be
              // reported for the redefinition and we don't need to properly
              // record dependency information for an invalid program.
              if (no->is_redefinition())
                continue;

	      // The subsequent vars have an implicit dependency on
	      // the first one, so that everything gets initialized in
	      // the right order and so that we detect cycles
	      // correctly.
	      this->gogo_->record_var_depends_on(no->var_value(), first_var);
	    }
	}
    }

  if (is_coloneq && !any_new)
    go_error_at(location, "variables redeclared but no variable is new");

  this->finish_init_vars(ivars, ivals, location);

  return true;
}

// See if we need to initialize a pair of values from a map index
// expression.  This returns true if we have set up the variables and
// the initialization.

bool
Parse::init_vars_from_map(const Typed_identifier_list* vars, Type* type,
			  Expression* expr, bool is_coloneq,
			  Location location)
{
  Index_expression* index = expr->index_expression();
  if (index == NULL)
    return false;
  if (vars->size() != 2)
    return false;

  // This is an index which is being assigned to two variables.  It
  // must be a map index.  Declare the variables, and then assign the
  // results of the map index.
  bool any_new = false;
  Typed_identifier_list::const_iterator p = vars->begin();
  Expression* init = type == NULL ? index : NULL;
  Named_object* val_no = this->init_var(*p, type, init, is_coloneq,
					type == NULL, &any_new, NULL, NULL);
  if (type == NULL && any_new && val_no->is_variable())
    val_no->var_value()->set_type_from_init_tuple();
  Expression* val_var = Expression::make_var_reference(val_no, location);

  ++p;
  Type* var_type = type;
  if (var_type == NULL)
    var_type = Type::lookup_bool_type();
  Named_object* no = this->init_var(*p, var_type, NULL, is_coloneq, false,
				    &any_new, NULL, NULL);
  Expression* present_var = Expression::make_var_reference(no, location);

  if (is_coloneq && !any_new)
    go_error_at(location, "variables redeclared but no variable is new");

  Statement* s = Statement::make_tuple_map_assignment(val_var, present_var,
						      index, location);

  if (!this->gogo_->in_global_scope())
    this->gogo_->add_statement(s);
  else if (!val_no->is_sink())
    {
      if (val_no->is_variable())
	{
	  val_no->var_value()->add_preinit_statement(this->gogo_, s);
	  if (no->is_variable())
	    this->gogo_->record_var_depends_on(no->var_value(), val_no);
	}
    }
  else if (!no->is_sink())
    {
      if (no->is_variable())
	no->var_value()->add_preinit_statement(this->gogo_, s);
    }
  else
    {
      // Execute the map index expression just so that we can fail if
      // the map is nil.
      Named_object* dummy = this->create_dummy_global(Type::lookup_bool_type(),
						      NULL, location);
      dummy->var_value()->add_preinit_statement(this->gogo_, s);
    }

  return true;
}

// See if we need to initialize a pair of values from a receive
// expression.  This returns true if we have set up the variables and
// the initialization.

bool
Parse::init_vars_from_receive(const Typed_identifier_list* vars, Type* type,
			      Expression* expr, bool is_coloneq,
			      Location location)
{
  Receive_expression* receive = expr->receive_expression();
  if (receive == NULL)
    return false;
  if (vars->size() != 2)
    return false;

  // This is a receive expression which is being assigned to two
  // variables.  Declare the variables, and then assign the results of
  // the receive.
  bool any_new = false;
  Typed_identifier_list::const_iterator p = vars->begin();
  Expression* init = type == NULL ? receive : NULL;
  Named_object* val_no = this->init_var(*p, type, init, is_coloneq,
					type == NULL, &any_new, NULL, NULL);
  if (type == NULL && any_new && val_no->is_variable())
    val_no->var_value()->set_type_from_init_tuple();
  Expression* val_var = Expression::make_var_reference(val_no, location);

  ++p;
  Type* var_type = type;
  if (var_type == NULL)
    var_type = Type::lookup_bool_type();
  Named_object* no = this->init_var(*p, var_type, NULL, is_coloneq, false,
				    &any_new, NULL, NULL);
  Expression* received_var = Expression::make_var_reference(no, location);

  if (is_coloneq && !any_new)
    go_error_at(location, "variables redeclared but no variable is new");

  Statement* s = Statement::make_tuple_receive_assignment(val_var,
							  received_var,
							  receive->channel(),
							  location);

  if (!this->gogo_->in_global_scope())
    this->gogo_->add_statement(s);
  else if (!val_no->is_sink())
    {
      if (val_no->is_variable())
	{
	  val_no->var_value()->add_preinit_statement(this->gogo_, s);
	  if (no->is_variable())
	    this->gogo_->record_var_depends_on(no->var_value(), val_no);
	}
    }
  else if (!no->is_sink())
    {
      if (no->is_variable())
	no->var_value()->add_preinit_statement(this->gogo_, s);
    }
  else
    {
      Named_object* dummy = this->create_dummy_global(Type::lookup_bool_type(),
						      NULL, location);
      dummy->var_value()->add_preinit_statement(this->gogo_, s);
    }

  return true;
}

// See if we need to initialize a pair of values from a type guard
// expression.  This returns true if we have set up the variables and
// the initialization.

bool
Parse::init_vars_from_type_guard(const Typed_identifier_list* vars,
				 Type* type, Expression* expr,
				 bool is_coloneq, Location location)
{
  Type_guard_expression* type_guard = expr->type_guard_expression();
  if (type_guard == NULL)
    return false;
  if (vars->size() != 2)
    return false;

  // This is a type guard expression which is being assigned to two
  // variables.  Declare the variables, and then assign the results of
  // the type guard.
  bool any_new = false;
  Typed_identifier_list::const_iterator p = vars->begin();
  Type* var_type = type;
  if (var_type == NULL)
    var_type = type_guard->type();
  Named_object* val_no = this->init_var(*p, var_type, NULL, is_coloneq, false,
					&any_new, NULL, NULL);
  Expression* val_var = Expression::make_var_reference(val_no, location);

  ++p;
  var_type = type;
  if (var_type == NULL)
    var_type = Type::lookup_bool_type();
  Named_object* no = this->init_var(*p, var_type, NULL, is_coloneq, false,
				    &any_new, NULL, NULL);
  Expression* ok_var = Expression::make_var_reference(no, location);

  Expression* texpr = type_guard->expr();
  Type* t = type_guard->type();
  Statement* s = Statement::make_tuple_type_guard_assignment(val_var, ok_var,
							     texpr, t,
							     location);

  if (is_coloneq && !any_new)
    go_error_at(location, "variables redeclared but no variable is new");

  if (!this->gogo_->in_global_scope())
    this->gogo_->add_statement(s);
  else if (!val_no->is_sink())
    {
      if (val_no->is_variable())
	{
	  val_no->var_value()->add_preinit_statement(this->gogo_, s);
	  if (no->is_variable())
	    this->gogo_->record_var_depends_on(no->var_value(), val_no);
	}
    }
  else if (!no->is_sink())
    {
      if (no->is_variable())
	no->var_value()->add_preinit_statement(this->gogo_, s);
    }
  else
    {
      Named_object* dummy = this->create_dummy_global(type, NULL, location);
      dummy->var_value()->add_preinit_statement(this->gogo_, s);
    }

  return true;
}

// Create a single variable.  If IS_COLONEQ is true, we permit
// redeclarations in the same block, and we set *IS_NEW when we find a
// new variable which is not a redeclaration.

Named_object*
Parse::init_var(const Typed_identifier& tid, Type* type, Expression* init,
		bool is_coloneq, bool type_from_init, bool* is_new,
		Expression_list* vars, Expression_list* vals)
{
  Location location = tid.location();

  if (Gogo::is_sink_name(tid.name()))
    {
      if (!type_from_init && init != NULL)
	{
	  if (this->gogo_->in_global_scope())
	    return this->create_dummy_global(type, init, location);
	  else
	    {
	      // Create a dummy variable so that we will check whether the
	      // initializer can be assigned to the type.
	      Variable* var = new Variable(type, init, false, false, false,
					   location);
	      var->set_is_used();
	      static int count;
	      char buf[30];
	      snprintf(buf, sizeof buf, "sink$%d", count);
	      ++count;
	      return this->gogo_->add_variable(buf, var);
	    }
	}
      if (type != NULL)
	this->gogo_->add_type_to_verify(type);
      return this->gogo_->add_sink();
    }

  if (is_coloneq)
    {
      Named_object* no = this->gogo_->lookup_in_block(tid.name());
      if (no != NULL
	  && (no->is_variable() || no->is_result_variable()))
	{
	  // INIT may be NULL even when IS_COLONEQ is true for cases
	  // like v, ok := x.(int).
	  if (!type_from_init && init != NULL)
	    {
	      go_assert(vars != NULL && vals != NULL);
	      vars->push_back(Expression::make_var_reference(no, location));
	      vals->push_back(init);
	    }
	  return no;
	}
    }
  *is_new = true;
  Variable* var = new Variable(type, init, this->gogo_->in_global_scope(),
			       false, false, location);
  Named_object* no = this->gogo_->add_variable(tid.name(), var);
  if (!no->is_variable())
    {
      // The name is already defined, so we just gave an error.
      return this->gogo_->add_sink();
    }
  return no;
}

// Create a dummy global variable to force an initializer to be run in
// the right place.  This is used when a sink variable is initialized
// at global scope.

Named_object*
Parse::create_dummy_global(Type* type, Expression* init,
			   Location location)
{
  if (type == NULL && init == NULL)
    type = Type::lookup_bool_type();
  Variable* var = new Variable(type, init, true, false, false, location);
  var->set_is_global_sink();
  static int count;
  char buf[30];
  snprintf(buf, sizeof buf, "_.%d", count);
  ++count;
  return this->gogo_->add_variable(buf, var);
}

// Finish the variable initialization by executing any assignments to
// existing variables when using :=.  These must be done as a tuple
// assignment in case of something like n, a, b := 1, b, a.

void
Parse::finish_init_vars(Expression_list* vars, Expression_list* vals,
			Location location)
{
  if (vars->empty())
    {
      delete vars;
      delete vals;
    }
  else if (vars->size() == 1)
    {
      go_assert(!this->gogo_->in_global_scope());
      this->gogo_->add_statement(Statement::make_assignment(vars->front(),
							    vals->front(),
							    location));
      delete vars;
      delete vals;
    }
  else
    {
      go_assert(!this->gogo_->in_global_scope());
      this->gogo_->add_statement(Statement::make_tuple_assignment(vars, vals,
								  location));
    }
}

// SimpleVarDecl = identifier ":=" Expression .

// We've already seen the identifier.

// FIXME: We also have to implement
//  IdentifierList ":=" ExpressionList
// In order to support both "a, b := 1, 0" and "a, b = 1, 0" we accept
// tuple assignments here as well.

// If MAY_BE_COMPOSITE_LIT is true, the expression on the right hand
// side may be a composite literal.

// If P_RANGE_CLAUSE is not NULL, then this will recognize a
// RangeClause.

// If P_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

void
Parse::simple_var_decl_or_assignment(const std::string& name,
				     Location location,
				     bool may_be_composite_lit,
				     Range_clause* p_range_clause,
				     Type_switch* p_type_switch)
{
  Typed_identifier_list til;
  til.push_back(Typed_identifier(name, NULL, location));

  std::set<std::string> uniq_idents;
  uniq_idents.insert(name);
  std::string dup_name;
  Location dup_loc;

  // We've seen one identifier.  If we see a comma now, this could be
  // "a, *p = 1, 2".
  if (this->peek_token()->is_op(OPERATOR_COMMA))
    {
      go_assert(p_type_switch == NULL);
      while (true)
	{
	  const Token* token = this->advance_token();
	  if (!token->is_identifier())
	    break;

	  std::string id = token->identifier();
	  bool is_id_exported = token->is_identifier_exported();
	  Location id_location = token->location();
	  std::pair<std::set<std::string>::iterator, bool> ins;

	  token = this->advance_token();
	  if (!token->is_op(OPERATOR_COMMA))
	    {
	      if (token->is_op(OPERATOR_COLONEQ))
		{
		  id = this->gogo_->pack_hidden_name(id, is_id_exported);
		  ins = uniq_idents.insert(id);
		  if (!ins.second && !Gogo::is_sink_name(id))
		    {
		      // Use %s to print := to avoid -Wformat-diag warning.
		      go_error_at(id_location,
				  "%qs repeated on left side of %s",
				  Gogo::message_name(id).c_str(), ":=");
		      id = this->gogo_->pack_hidden_name("_", false);
		    }
		  til.push_back(Typed_identifier(id, NULL, location));
		}
	      else
		this->unget_token(Token::make_identifier_token(id,
							       is_id_exported,
							       id_location));
	      break;
	    }

	  id = this->gogo_->pack_hidden_name(id, is_id_exported);
	  ins = uniq_idents.insert(id);
	  std::string name = id;
	  if (!ins.second && !Gogo::is_sink_name(id))
	    {
	      dup_name = Gogo::message_name(id);
	      dup_loc = id_location;
	      id = this->gogo_->pack_hidden_name("_", false);
	    }
	  til.push_back(Typed_identifier(id, NULL, location));
	}

      // We have a comma separated list of identifiers in TIL.  If the
      // next token is COLONEQ, then this is a simple var decl, and we
      // have the complete list of identifiers.  If the next token is
      // not COLONEQ, then the only valid parse is a tuple assignment.
      // The list of identifiers we have so far is really a list of
      // expressions.  There are more expressions following.

      if (!this->peek_token()->is_op(OPERATOR_COLONEQ))
	{
	  Expression_list* exprs = new Expression_list;
	  for (Typed_identifier_list::const_iterator p = til.begin();
	       p != til.end();
	       ++p)
	    exprs->push_back(this->id_to_expression(p->name(), p->location(),
						    true, false));

	  Expression_list* more_exprs =
	    this->expression_list(NULL, true, may_be_composite_lit);
	  for (Expression_list::const_iterator p = more_exprs->begin();
	       p != more_exprs->end();
	       ++p)
	    exprs->push_back(*p);
	  delete more_exprs;

	  this->tuple_assignment(exprs, may_be_composite_lit, p_range_clause);
	  return;
	}
    }

  go_assert(this->peek_token()->is_op(OPERATOR_COLONEQ));
  const Token* token = this->advance_token();

  if (!dup_name.empty())
    {
      // Use %s to print := to avoid -Wformat-diag warning.
      go_error_at(dup_loc, "%qs repeated on left side of %s",
		  dup_name.c_str(), ":=");
    }

  if (p_range_clause != NULL && token->is_keyword(KEYWORD_RANGE))
    {
      this->range_clause_decl(&til, p_range_clause);
      return;
    }

  Expression_list* init;
  if (p_type_switch == NULL)
    init = this->expression_list(NULL, false, may_be_composite_lit);
  else
    {
      bool is_type_switch = false;
      Expression* expr = this->expression(PRECEDENCE_NORMAL, false,
					  may_be_composite_lit,
					  &is_type_switch, NULL);
      if (is_type_switch)
	{
	  p_type_switch->found = true;
	  p_type_switch->name = name;
	  p_type_switch->location = location;
	  p_type_switch->expr = expr;
	  return;
	}

      if (!this->peek_token()->is_op(OPERATOR_COMMA))
	{
	  init = new Expression_list();
	  init->push_back(expr);
	}
      else
	{
	  this->advance_token();
	  init = this->expression_list(expr, false, may_be_composite_lit);
	}
    }

  this->init_vars(&til, NULL, init, true, NULL, location);
}

// FunctionDecl = "func" identifier Signature [ Block ] .
// MethodDecl = "func" Receiver identifier Signature [ Block ] .

// Deprecated gcc extension:
//   FunctionDecl = "func" identifier Signature
//                    __asm__ "(" string_lit ")" .
// This extension means a function whose real name is the identifier
// inside the asm.  This extension will be removed at some future
// date.  It has been replaced with //extern or //go:linkname comments.
//
// PRAGMAS is a bitset of magic comments.

void
Parse::function_decl()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_FUNC));

  unsigned int pragmas = this->lex_->get_and_clear_pragmas();
  this->check_directives();

  Location location = this->location();
  std::string extern_name = this->lex_->extern_name();
  const Token* token = this->advance_token();

  bool expected_receiver = false;
  Typed_identifier* rec = NULL;
  if (token->is_op(OPERATOR_LPAREN))
    {
      expected_receiver = true;
      rec = this->receiver();
      token = this->peek_token();
    }

  if (!token->is_identifier())
    {
      go_error_at(this->location(), "expected function name");
      return;
    }

  std::string name =
    this->gogo_->pack_hidden_name(token->identifier(),
				  token->is_identifier_exported());

  this->advance_token();

  Function_type* fntype = this->signature(rec, this->location());

  Named_object* named_object = NULL;

  if (this->peek_token()->is_keyword(KEYWORD_ASM))
    {
      if (!this->advance_token()->is_op(OPERATOR_LPAREN))
	{
	  go_error_at(this->location(), "expected %<(%>");
	  return;
	}
      token = this->advance_token();
      if (!token->is_string())
	{
	  go_error_at(this->location(), "expected string");
	  return;
	}
      std::string asm_name = token->string_value();
      if (!this->advance_token()->is_op(OPERATOR_RPAREN))
	{
	  go_error_at(this->location(), "expected %<)%>");
	  return;
	}
      this->advance_token();
      if (!Gogo::is_sink_name(name))
	{
	  named_object = this->gogo_->declare_function(name, fntype, location);
	  if (named_object->is_function_declaration())
	    named_object->func_declaration_value()->set_asm_name(asm_name);
	}
    }

  // Check for the easy error of a newline before the opening brace.
  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      Location semi_loc = this->location();
      if (this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(this->location(),
		    "unexpected semicolon or newline before %<{%>");
      else
	this->unget_token(Token::make_operator_token(OPERATOR_SEMICOLON,
						     semi_loc));
    }

  static struct {
    unsigned int bit;
    const char* name;
    bool decl_ok;
    bool func_ok;
    bool method_ok;
  } pragma_check[] =
      {
	{ GOPRAGMA_NOINTERFACE, "nointerface", false, false, true },
	{ GOPRAGMA_NOESCAPE, "noescape", true, false, false },
	{ GOPRAGMA_NORACE, "norace", false, true, true },
	{ GOPRAGMA_NOSPLIT, "nosplit", false, true, true },
	{ GOPRAGMA_NOINLINE, "noinline", false, true, true },
	{ GOPRAGMA_SYSTEMSTACK, "systemstack", false, true, true },
	{ GOPRAGMA_NOWRITEBARRIER, "nowritebarrier", false, true, true },
	{ GOPRAGMA_NOWRITEBARRIERREC, "nowritebarrierrec", false, true,
	  true },
	{ GOPRAGMA_YESWRITEBARRIERREC, "yeswritebarrierrec", false, true,
	  true },
	{ GOPRAGMA_CGOUNSAFEARGS, "cgo_unsafe_args", false, true, true },
	{ GOPRAGMA_UINTPTRESCAPES, "uintptrescapes", true, true, true },
      };

  bool is_decl = !this->peek_token()->is_op(OPERATOR_LCURLY);
  if (pragmas != 0)
    {
      for (size_t i = 0;
	   i < sizeof(pragma_check) / sizeof(pragma_check[0]);
	   ++i)
	{
	  if ((pragmas & pragma_check[i].bit) == 0)
	    continue;

	  if (is_decl)
	    {
	      if (pragma_check[i].decl_ok)
		continue;
	      go_warning_at(location, 0,
			    ("ignoring magic %<//go:%s%> comment "
			     "before declaration"),
			    pragma_check[i].name);
	    }
	  else if (rec == NULL)
	    {
	      if (pragma_check[i].func_ok)
		continue;
	      go_warning_at(location, 0,
			    ("ignoring magic %<//go:%s%> comment "
			     "before function definition"),
			    pragma_check[i].name);
	    }
	  else
	    {
	      if (pragma_check[i].method_ok)
		continue;
	      go_warning_at(location, 0,
			    ("ignoring magic %<//go:%s%> comment "
			     "before method definition"),
			    pragma_check[i].name);
	    }

	  pragmas &= ~ pragma_check[i].bit;
	}
    }

  if (is_decl)
    {
      if (named_object == NULL)
	{
          // Function declarations with the blank identifier as a name are
          // mostly ignored since they cannot be called.  We make an object
          // for this declaration for type-checking purposes.
          if (Gogo::is_sink_name(name))
            {
              static int count;
              char buf[30];
              snprintf(buf, sizeof buf, ".$sinkfndecl%d", count);
              ++count;
              name = std::string(buf);
            }

	  if (fntype == NULL
              || (expected_receiver && rec == NULL))
	    this->gogo_->add_erroneous_name(name);
	  else
	    {
	      named_object = this->gogo_->declare_function(name, fntype,
							   location);
	      if (!extern_name.empty()
		  && named_object->is_function_declaration())
		{
		  Function_declaration* fd =
		    named_object->func_declaration_value();
		  fd->set_asm_name(extern_name);
		}
	    }
	}

      if (pragmas != 0 && named_object->is_function_declaration())
	named_object->func_declaration_value()->set_pragmas(pragmas);
    }
  else
    {
      bool hold_is_erroneous_function = this->is_erroneous_function_;
      if (fntype == NULL)
	{
	  fntype = Type::make_function_type(NULL, NULL, NULL, location);
	  this->is_erroneous_function_ = true;
	  if (!Gogo::is_sink_name(name))
	    this->gogo_->add_erroneous_name(name);
	  name = this->gogo_->pack_hidden_name("_", false);
	}
      named_object = this->gogo_->start_function(name, fntype, true, location);
      Location end_loc = this->block();
      this->gogo_->finish_function(end_loc);

      if (pragmas != 0
	  && !this->is_erroneous_function_
	  && named_object->is_function())
	named_object->func_value()->set_pragmas(pragmas);
      this->is_erroneous_function_ = hold_is_erroneous_function;
    }
}

// Receiver = Parameters .

Typed_identifier*
Parse::receiver()
{
  Location location = this->location();
  Typed_identifier_list* til;
  if (!this->parameters(&til, NULL))
    return NULL;
  else if (til == NULL || til->empty())
    {
      go_error_at(location, "method has no receiver");
      return NULL;
    }
  else if (til->size() > 1)
    {
      go_error_at(location, "method has multiple receivers");
      return NULL;
    }
  else
    return &til->front();
}

// Operand    = Literal | QualifiedIdent | MethodExpr | "(" Expression ")" .
// Literal    = BasicLit | CompositeLit | FunctionLit .
// BasicLit   = int_lit | float_lit | imaginary_lit | char_lit | string_lit .

// If MAY_BE_SINK is true, this operand may be "_".

// If IS_PARENTHESIZED is not NULL, *IS_PARENTHESIZED is set to true
// if the entire expression is in parentheses.

Expression*
Parse::operand(bool may_be_sink, bool* is_parenthesized)
{
  const Token* token = this->peek_token();
  Expression* ret;
  switch (token->classification())
    {
    case Token::TOKEN_IDENTIFIER:
      {
	Location location = token->location();
	std::string id = token->identifier();
	bool is_exported = token->is_identifier_exported();
	std::string packed = this->gogo_->pack_hidden_name(id, is_exported);

	Named_object* in_function;
	Named_object* named_object = this->gogo_->lookup(packed, &in_function);

	Package* package = NULL;
	if (named_object != NULL && named_object->is_package())
	  {
	    if (!this->advance_token()->is_op(OPERATOR_DOT)
		|| !this->advance_token()->is_identifier())
	      {
		go_error_at(location, "unexpected reference to package");
		return Expression::make_error(location);
	      }
	    package = named_object->package_value();
	    package->note_usage(id);
	    id = this->peek_token()->identifier();
	    is_exported = this->peek_token()->is_identifier_exported();
	    packed = this->gogo_->pack_hidden_name(id, is_exported);
	    named_object = package->lookup(packed);
	    location = this->location();
	    go_assert(in_function == NULL);
	  }

	this->advance_token();

	if (named_object != NULL
	    && named_object->is_type()
	    && !named_object->type_value()->is_visible())
	  {
	    go_assert(package != NULL);
	    go_error_at(location, "invalid reference to hidden type %<%s.%s%>",
			Gogo::message_name(package->package_name()).c_str(),
			Gogo::message_name(id).c_str());
	    return Expression::make_error(location);
	  }


	if (named_object == NULL)
	  {
	    if (package != NULL)
	      {
		std::string n1 = Gogo::message_name(package->package_name());
		std::string n2 = Gogo::message_name(id);
		if (!is_exported)
		  go_error_at(location,
			      ("invalid reference to unexported identifier "
			       "%<%s.%s%>"),
			      n1.c_str(), n2.c_str());
		else
		  go_error_at(location,
			      "reference to undefined identifier %<%s.%s%>",
			      n1.c_str(), n2.c_str());
		return Expression::make_error(location);
	      }

	    named_object = this->gogo_->add_unknown_name(packed, location);
	  }

	if (in_function != NULL
	    && in_function != this->gogo_->current_function()
	    && (named_object->is_variable()
		|| named_object->is_result_variable()))
	  return this->enclosing_var_reference(in_function, named_object,
					       may_be_sink, location);

	switch (named_object->classification())
	  {
	  case Named_object::NAMED_OBJECT_CONST:
	    return Expression::make_const_reference(named_object, location);
	  case Named_object::NAMED_OBJECT_TYPE:
	    return Expression::make_type(named_object->type_value(), location);
	  case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
	    {
	      Type* t = Type::make_forward_declaration(named_object);
	      return Expression::make_type(t, location);
	    }
	  case Named_object::NAMED_OBJECT_VAR:
	  case Named_object::NAMED_OBJECT_RESULT_VAR:
	    // Any left-hand-side can be a sink, so if this can not be
	    // a sink, then it must be a use of the variable.
	    if (!may_be_sink)
	      this->mark_var_used(named_object);
	    return Expression::make_var_reference(named_object, location);
	  case Named_object::NAMED_OBJECT_SINK:
	    if (may_be_sink)
	      return Expression::make_sink(location);
	    else
	      {
		go_error_at(location, "cannot use %<_%> as value");
		return Expression::make_error(location);
	      }
	  case Named_object::NAMED_OBJECT_FUNC:
	  case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
	    return Expression::make_func_reference(named_object, NULL,
						   location);
	  case Named_object::NAMED_OBJECT_UNKNOWN:
	    {
	      Unknown_expression* ue =
		Expression::make_unknown_reference(named_object, location);
	      if (this->is_erroneous_function_)
		ue->set_no_error_message();
	      return ue;
	    }
	  case Named_object::NAMED_OBJECT_ERRONEOUS:
	    return Expression::make_error(location);
	  default:
	    go_unreachable();
	  }
      }
      go_unreachable();

    case Token::TOKEN_STRING:
      ret = Expression::make_string(token->string_value(), token->location());
      this->advance_token();
      return ret;

    case Token::TOKEN_CHARACTER:
      ret = Expression::make_character(token->character_value(), NULL,
				       token->location());
      this->advance_token();
      return ret;

    case Token::TOKEN_INTEGER:
      ret = Expression::make_integer_z(token->integer_value(), NULL,
				       token->location());
      this->advance_token();
      return ret;

    case Token::TOKEN_FLOAT:
      ret = Expression::make_float(token->float_value(), NULL,
				   token->location());
      this->advance_token();
      return ret;

    case Token::TOKEN_IMAGINARY:
      {
	mpfr_t zero;
	mpfr_init_set_ui(zero, 0, MPFR_RNDN);
	mpc_t val;
	mpc_init2(val, mpc_precision);
	mpc_set_fr_fr(val, zero, *token->imaginary_value(), MPC_RNDNN);
	mpfr_clear(zero);
	ret = Expression::make_complex(&val, NULL, token->location());
	mpc_clear(val);
	this->advance_token();
	return ret;
      }

    case Token::TOKEN_KEYWORD:
      switch (token->keyword())
	{
	case KEYWORD_FUNC:
	  return this->function_lit();
	case KEYWORD_CHAN:
	case KEYWORD_INTERFACE:
	case KEYWORD_MAP:
	case KEYWORD_STRUCT:
	  {
	    Location location = token->location();
	    return Expression::make_type(this->type(), location);
	  }
	default:
	  break;
	}
      break;

    case Token::TOKEN_OPERATOR:
      if (token->is_op(OPERATOR_LPAREN))
	{
	  this->advance_token();
	  ret = this->expression(PRECEDENCE_NORMAL, may_be_sink, true, NULL,
				 NULL);
	  if (!this->peek_token()->is_op(OPERATOR_RPAREN))
	    go_error_at(this->location(), "missing %<)%>");
	  else
	    this->advance_token();
	  if (is_parenthesized != NULL)
	    *is_parenthesized = true;
	  return ret;
	}
      else if (token->is_op(OPERATOR_LSQUARE))
	{
	  // Here we call array_type directly, as this is the only
	  // case where an ellipsis is permitted for an array type.
	  Location location = token->location();
	  return Expression::make_type(this->array_type(true), location);
	}
      break;

    default:
      break;
    }

  go_error_at(this->location(), "expected operand");
  return Expression::make_error(this->location());
}

// Handle a reference to a variable in an enclosing function.  We add
// it to a list of such variables.  We return a reference to a field
// in a struct which will be passed on the static chain when calling
// the current function.

Expression*
Parse::enclosing_var_reference(Named_object* in_function, Named_object* var,
			       bool may_be_sink, Location location)
{
  go_assert(var->is_variable() || var->is_result_variable());

  // Any left-hand-side can be a sink, so if this can not be
  // a sink, then it must be a use of the variable.
  if (!may_be_sink)
    this->mark_var_used(var);

  Named_object* this_function = this->gogo_->current_function();
  Named_object* closure = this_function->func_value()->closure_var();

  // The last argument to the Enclosing_var constructor is the index
  // of this variable in the closure.  We add 1 to the current number
  // of enclosed variables, because the first field in the closure
  // points to the function code.
  Enclosing_var ev(var, in_function, this->enclosing_vars_.size() + 1);
  std::pair<Enclosing_vars::iterator, bool> ins =
    this->enclosing_vars_.insert(ev);
  if (ins.second)
    {
      // This is a variable we have not seen before.  Add a new field
      // to the closure type.
      this_function->func_value()->add_closure_field(var, location);
    }

  Expression* closure_ref = Expression::make_var_reference(closure,
							   location);
  closure_ref =
      Expression::make_dereference(closure_ref,
                                   Expression::NIL_CHECK_NOT_NEEDED,
                                   location);

  // The closure structure holds pointers to the variables, so we need
  // to introduce an indirection.
  Expression* e = Expression::make_field_reference(closure_ref,
						   ins.first->index(),
						   location);
  e = Expression::make_dereference(e, Expression::NIL_CHECK_NOT_NEEDED,
                                   location);
  return Expression::make_enclosing_var_reference(e, var, location);
}

// CompositeLit  = LiteralType LiteralValue .
// LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
//                 SliceType | MapType | TypeName .
// LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
// ElementList   = Element { "," Element } .
// Element       = [ Key ":" ] Value .
// Key           = FieldName | ElementIndex .
// FieldName     = identifier .
// ElementIndex  = Expression .
// Value         = Expression | LiteralValue .

// We have already seen the type if there is one, and we are now
// looking at the LiteralValue.  The case "[" "..."  "]" ElementType
// will be seen here as an array type whose length is "nil".  The
// DEPTH parameter is non-zero if this is an embedded composite
// literal and the type was omitted.  It gives the number of steps up
// to the type which was provided.  E.g., in [][]int{{1}} it will be
// 1.  In [][][]int{{{1}}} it will be 2.

Expression*
Parse::composite_lit(Type* type, int depth, Location location)
{
  go_assert(this->peek_token()->is_op(OPERATOR_LCURLY));
  this->advance_token();

  if (this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      this->advance_token();
      return Expression::make_composite_literal(type, depth, false, NULL,
						false, location);
    }

  bool has_keys = false;
  bool all_are_names = true;
  Expression_list* vals = new Expression_list;
  while (true)
    {
      Expression* val;
      bool is_type_omitted = false;
      bool is_name = false;

      const Token* token = this->peek_token();

      if (token->is_identifier())
	{
	  std::string identifier = token->identifier();
	  bool is_exported = token->is_identifier_exported();
	  Location id_location = token->location();

	  if (this->advance_token()->is_op(OPERATOR_COLON))
	    {
	      // This may be a field name.  We don't know for sure--it
	      // could also be an expression for an array index.  We
	      // don't want to parse it as an expression because may
	      // trigger various errors, e.g., if this identifier
	      // happens to be the name of a package.
	      Gogo* gogo = this->gogo_;
	      val = this->id_to_expression(gogo->pack_hidden_name(identifier,
								  is_exported),
					   id_location, false, true);
	      is_name = true;
	    }
	  else
	    {
	      this->unget_token(Token::make_identifier_token(identifier,
							     is_exported,
							     id_location));
	      val = this->expression(PRECEDENCE_NORMAL, false, true, NULL,
				     NULL);
	    }
	}
      else if (!token->is_op(OPERATOR_LCURLY))
	val = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
      else
	{
	  // This must be a composite literal inside another composite
	  // literal, with the type omitted for the inner one.
	  val = this->composite_lit(type, depth + 1, token->location());
          is_type_omitted = true;
	}

      token = this->peek_token();
      if (!token->is_op(OPERATOR_COLON))
	{
	  if (has_keys)
	    vals->push_back(NULL);
	  is_name = false;
	}
      else
	{
          if (is_type_omitted)
            {
              // VAL is a nested composite literal with an omitted type being
              // used a key.  Record this information in VAL so that the correct
              // type is associated with the literal value if VAL is a
              // map literal.
              val->complit()->update_key_path(depth);
            }

	  this->advance_token();

	  if (!has_keys && !vals->empty())
	    {
	      Expression_list* newvals = new Expression_list;
	      for (Expression_list::const_iterator p = vals->begin();
		   p != vals->end();
		   ++p)
		{
		  newvals->push_back(NULL);
		  newvals->push_back(*p);
		}
	      delete vals;
	      vals = newvals;
	    }
	  has_keys = true;

	  vals->push_back(val);

	  if (!token->is_op(OPERATOR_LCURLY))
	    val = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
	  else
	    {
	      // This must be a composite literal inside another
	      // composite literal, with the type omitted for the
	      // inner one.
	      val = this->composite_lit(type, depth + 1, token->location());
	    }

	  token = this->peek_token();
	}

      vals->push_back(val);

      if (!is_name)
	all_are_names = false;

      if (token->is_op(OPERATOR_COMMA))
	{
	  if (this->advance_token()->is_op(OPERATOR_RCURLY))
	    {
	      this->advance_token();
	      break;
	    }
	}
      else if (token->is_op(OPERATOR_RCURLY))
	{
	  this->advance_token();
	  break;
	}
      else
	{
	  if (token->is_op(OPERATOR_SEMICOLON))
	    go_error_at(this->location(),
			("need trailing comma before newline "
			 "in composite literal"));
	  else
	    go_error_at(this->location(), "expected %<,%> or %<}%>");

	  this->gogo_->mark_locals_used();
	  int edepth = 0;
	  while (!token->is_eof()
		 && (edepth > 0 || !token->is_op(OPERATOR_RCURLY)))
	    {
	      if (token->is_op(OPERATOR_LCURLY))
		++edepth;
	      else if (token->is_op(OPERATOR_RCURLY))
		--edepth;
	      token = this->advance_token();
	    }
	  if (token->is_op(OPERATOR_RCURLY))
	    this->advance_token();

	  return Expression::make_error(location);
	}
    }

  return Expression::make_composite_literal(type, depth, has_keys, vals,
					    all_are_names, location);
}

// FunctionLit = "func" Signature Block .

Expression*
Parse::function_lit()
{
  Location location = this->location();
  go_assert(this->peek_token()->is_keyword(KEYWORD_FUNC));
  this->advance_token();

  Enclosing_vars hold_enclosing_vars;
  hold_enclosing_vars.swap(this->enclosing_vars_);

  Function_type* type = this->signature(NULL, location);
  bool fntype_is_error = false;
  if (type == NULL)
    {
      type = Type::make_function_type(NULL, NULL, NULL, location);
      fntype_is_error = true;
    }

  // For a function literal, the next token must be a '{'.  If we
  // don't see that, then we may have a type expression.
  if (!this->peek_token()->is_op(OPERATOR_LCURLY))
    {
      hold_enclosing_vars.swap(this->enclosing_vars_);
      return Expression::make_type(type, location);
    }

  bool hold_is_erroneous_function = this->is_erroneous_function_;
  if (fntype_is_error)
    this->is_erroneous_function_ = true;

  Bc_stack* hold_break_stack = this->break_stack_;
  Bc_stack* hold_continue_stack = this->continue_stack_;
  this->break_stack_ = NULL;
  this->continue_stack_ = NULL;

  Named_object* no = this->gogo_->start_function("", type, true, location);

  Location end_loc = this->block();

  this->gogo_->finish_function(end_loc);

  if (this->break_stack_ != NULL)
    delete this->break_stack_;
  if (this->continue_stack_ != NULL)
    delete this->continue_stack_;
  this->break_stack_ = hold_break_stack;
  this->continue_stack_ = hold_continue_stack;

  this->is_erroneous_function_ = hold_is_erroneous_function;

  hold_enclosing_vars.swap(this->enclosing_vars_);

  Expression* closure = this->create_closure(no, &hold_enclosing_vars,
					     location);

  return Expression::make_func_reference(no, closure, location);
}

// Create a closure for the nested function FUNCTION.  This is based
// on ENCLOSING_VARS, which is a list of all variables defined in
// enclosing functions and referenced from FUNCTION.  A closure is the
// address of a struct which point to the real function code and
// contains the addresses of all the referenced variables.  This
// returns NULL if no closure is required.

Expression*
Parse::create_closure(Named_object* function, Enclosing_vars* enclosing_vars,
		      Location location)
{
  if (enclosing_vars->empty())
    return NULL;

  // Get the variables in order by their field index.

  size_t enclosing_var_count = enclosing_vars->size();
  std::vector<Enclosing_var> ev(enclosing_var_count);
  for (Enclosing_vars::const_iterator p = enclosing_vars->begin();
       p != enclosing_vars->end();
       ++p)
    {
      // Subtract 1 because index 0 is the function code.
      ev[p->index() - 1] = *p;
    }

  // Build an initializer for a composite literal of the closure's
  // type.

  Named_object* enclosing_function = this->gogo_->current_function();
  Expression_list* initializer = new Expression_list;

  initializer->push_back(Expression::make_func_code_reference(function,
							      location));

  for (size_t i = 0; i < enclosing_var_count; ++i)
    {
      // Add 1 to i because the first field in the closure is a
      // pointer to the function code.
      go_assert(ev[i].index() == i + 1);
      Named_object* var = ev[i].var();
      Expression* ref;
      if (ev[i].in_function() == enclosing_function)
	ref = Expression::make_var_reference(var, location);
      else
	ref = this->enclosing_var_reference(ev[i].in_function(), var,
					    true, location);
      Expression* refaddr = Expression::make_unary(OPERATOR_AND, ref,
						   location);
      initializer->push_back(refaddr);
    }

  Named_object* closure_var = function->func_value()->closure_var();
  Struct_type* st = closure_var->var_value()->type()->deref()->struct_type();
  Expression* cv = Expression::make_struct_composite_literal(st, initializer,
							     location);
  return Expression::make_heap_expression(cv, location);
}

// PrimaryExpr = Operand { Selector | Index | Slice | TypeGuard | Call } .

// If MAY_BE_SINK is true, this expression may be "_".

// If MAY_BE_COMPOSITE_LIT is true, this expression may be a composite
// literal.

// If IS_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

// If IS_PARENTHESIZED is not NULL, *IS_PARENTHESIZED is set to true
// if the entire expression is in parentheses.

Expression*
Parse::primary_expr(bool may_be_sink, bool may_be_composite_lit,
		    bool* is_type_switch, bool* is_parenthesized)
{
  Location start_loc = this->location();
  bool operand_is_parenthesized = false;
  bool whole_is_parenthesized = false;

  Expression* ret = this->operand(may_be_sink, &operand_is_parenthesized);

  whole_is_parenthesized = operand_is_parenthesized;

  // An unknown name followed by a curly brace must be a composite
  // literal, and the unknown name must be a type.
  if (may_be_composite_lit
      && !operand_is_parenthesized
      && ret->unknown_expression() != NULL
      && this->peek_token()->is_op(OPERATOR_LCURLY))
    {
      Named_object* no = ret->unknown_expression()->named_object();
      Type* type = Type::make_forward_declaration(no);
      ret = Expression::make_type(type, ret->location());
    }

  // We handle composite literals and type casts here, as it is the
  // easiest way to handle types which are in parentheses, as in
  // "((uint))(1)".
  if (ret->is_type_expression())
    {
      if (this->peek_token()->is_op(OPERATOR_LCURLY))
	{
	  whole_is_parenthesized = false;
	  if (!may_be_composite_lit)
	    {
	      Type* t = ret->type();
	      if (t->named_type() != NULL
		  || t->forward_declaration_type() != NULL)
		go_error_at(start_loc,
			    _("parentheses required around this composite "
			      "literal to avoid parsing ambiguity"));
	    }
	  else if (operand_is_parenthesized)
	    go_error_at(start_loc,
			"cannot parenthesize type in composite literal");
	  ret = this->composite_lit(ret->type(), 0, ret->location());
	}
      else if (this->peek_token()->is_op(OPERATOR_LPAREN))
	{
	  whole_is_parenthesized = false;
	  Location loc = this->location();
	  this->advance_token();
	  Expression* expr = this->expression(PRECEDENCE_NORMAL, false, true,
					      NULL, NULL);
	  if (this->peek_token()->is_op(OPERATOR_COMMA))
	    this->advance_token();
	  if (this->peek_token()->is_op(OPERATOR_ELLIPSIS))
	    {
	      go_error_at(this->location(),
			  "invalid use of %<...%> in type conversion");
	      this->advance_token();
	    }
	  if (!this->peek_token()->is_op(OPERATOR_RPAREN))
	    go_error_at(this->location(), "expected %<)%>");
	  else
	    this->advance_token();
	  if (expr->is_error_expression())
	    ret = expr;
	  else
	    {
	      Type* t = ret->type();
	      if (t->classification() == Type::TYPE_ARRAY
		  && t->array_type()->length() != NULL
		  && t->array_type()->length()->is_nil_expression())
		{
		  go_error_at(ret->location(),
			      "use of %<[...]%> outside of array literal");
		  ret = Expression::make_error(loc);
		}
	      else
		ret = Expression::make_cast(t, expr, loc);
	    }
	}
    }

  while (true)
    {
      const Token* token = this->peek_token();
      if (token->is_op(OPERATOR_LPAREN))
	{
	  whole_is_parenthesized = false;
	  ret = this->call(this->verify_not_sink(ret));
	}
      else if (token->is_op(OPERATOR_DOT))
	{
	  whole_is_parenthesized = false;
	  ret = this->selector(this->verify_not_sink(ret), is_type_switch);
	  if (is_type_switch != NULL && *is_type_switch)
	    break;
	}
      else if (token->is_op(OPERATOR_LSQUARE))
	{
	  whole_is_parenthesized = false;
	  ret = this->index(this->verify_not_sink(ret));
	}
      else
	break;
    }

  if (whole_is_parenthesized && is_parenthesized != NULL)
    *is_parenthesized = true;

  return ret;
}

// Selector = "." identifier .
// TypeGuard = "." "(" QualifiedIdent ")" .

// Note that Operand can expand to QualifiedIdent, which contains a
// ".".  That is handled directly in operand when it sees a package
// name.

// If IS_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

Expression*
Parse::selector(Expression* left, bool* is_type_switch)
{
  go_assert(this->peek_token()->is_op(OPERATOR_DOT));
  Location location = this->location();

  const Token* token = this->advance_token();
  if (token->is_identifier())
    {
      // This could be a field in a struct, or a method in an
      // interface, or a method associated with a type.  We can't know
      // which until we have seen all the types.
      std::string name =
	this->gogo_->pack_hidden_name(token->identifier(),
				      token->is_identifier_exported());
      if (token->identifier() == "_")
	{
	  go_error_at(this->location(), "invalid use of %<_%>");
	  name = Gogo::erroneous_name();
	}
      this->advance_token();
      return Expression::make_selector(left, name, location);
    }
  else if (token->is_op(OPERATOR_LPAREN))
    {
      this->advance_token();
      Type* type = NULL;
      if (!this->peek_token()->is_keyword(KEYWORD_TYPE))
	type = this->type();
      else
	{
	  if (is_type_switch != NULL)
	    *is_type_switch = true;
	  else
	    {
	      go_error_at(this->location(),
			  "use of %<.(type)%> outside type switch");
	      type = Type::make_error_type();
	    }
	  this->advance_token();
	}
      if (!this->peek_token()->is_op(OPERATOR_RPAREN))
	go_error_at(this->location(), "missing %<)%>");
      else
	this->advance_token();
      if (is_type_switch != NULL && *is_type_switch)
	return left;
      return Expression::make_type_guard(left, type, location);
    }
  else
    {
      go_error_at(this->location(), "expected identifier or %<(%>");
      return left;
    }
}

// Index          = "[" Expression "]" .
// Slice          = "[" Expression ":" [ Expression ] [ ":" Expression ] "]" .

Expression*
Parse::index(Expression* expr)
{
  Location location = this->location();
  go_assert(this->peek_token()->is_op(OPERATOR_LSQUARE));
  this->advance_token();

  Expression* start;
  if (!this->peek_token()->is_op(OPERATOR_COLON))
    start = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
  else
    start = Expression::make_integer_ul(0, NULL, location);

  Expression* end = NULL;
  if (this->peek_token()->is_op(OPERATOR_COLON))
    {
      // We use nil to indicate a missing high expression.
      if (this->advance_token()->is_op(OPERATOR_RSQUARE))
	end = Expression::make_nil(this->location());
      else if (this->peek_token()->is_op(OPERATOR_COLON))
	{
	  go_error_at(this->location(),
		      "middle index required in 3-index slice");
	  end = Expression::make_error(this->location());
	}
      else
	end = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
    }

  Expression* cap = NULL;
  if (this->peek_token()->is_op(OPERATOR_COLON))
    {
      if (this->advance_token()->is_op(OPERATOR_RSQUARE))
	{
	  go_error_at(this->location(),
		      "final index required in 3-index slice");
	  cap = Expression::make_error(this->location());
	}
      else
        cap = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
    }
  if (!this->peek_token()->is_op(OPERATOR_RSQUARE))
    go_error_at(this->location(), "missing %<]%>");
  else
    this->advance_token();
  return Expression::make_index(expr, start, end, cap, location);
}

// Call           = "(" [ ArgumentList [ "," ] ] ")" .
// ArgumentList   = ExpressionList [ "..." ] .

Expression*
Parse::call(Expression* func)
{
  go_assert(this->peek_token()->is_op(OPERATOR_LPAREN));
  Expression_list* args = NULL;
  bool is_varargs = false;
  const Token* token = this->advance_token();
  if (!token->is_op(OPERATOR_RPAREN))
    {
      args = this->expression_list(NULL, false, true);
      token = this->peek_token();
      if (token->is_op(OPERATOR_ELLIPSIS))
	{
	  is_varargs = true;
	  token = this->advance_token();
	}
    }
  if (token->is_op(OPERATOR_COMMA))
    token = this->advance_token();
  if (!token->is_op(OPERATOR_RPAREN))
    {
      go_error_at(this->location(), "missing %<)%>");
      if (!this->skip_past_error(OPERATOR_RPAREN))
	return Expression::make_error(this->location());
    }
  this->advance_token();
  if (func->is_error_expression())
    return func;
  return Expression::make_call(func, args, is_varargs, func->location());
}

// Return an expression for a single unqualified identifier.

Expression*
Parse::id_to_expression(const std::string& name, Location location,
			bool is_lhs, bool is_composite_literal_key)
{
  Named_object* in_function;
  Named_object* named_object = this->gogo_->lookup(name, &in_function);
  if (named_object == NULL)
    {
      if (is_composite_literal_key)
	{
	  // This is a composite literal key, which means that it
	  // could just be a struct field name, so avoid confusion by
	  // not adding it to the bindings.  We'll look up the name
	  // later during the determine types phase if necessary.
	  return Expression::make_composite_literal_key(name, location);
	}
      named_object = this->gogo_->add_unknown_name(name, location);
    }

  if (in_function != NULL
      && in_function != this->gogo_->current_function()
      && (named_object->is_variable() || named_object->is_result_variable()))
    return this->enclosing_var_reference(in_function, named_object, is_lhs,
					 location);

  switch (named_object->classification())
    {
    case Named_object::NAMED_OBJECT_CONST:
      return Expression::make_const_reference(named_object, location);
    case Named_object::NAMED_OBJECT_VAR:
    case Named_object::NAMED_OBJECT_RESULT_VAR:
      if (!is_lhs)
	this->mark_var_used(named_object);
      return Expression::make_var_reference(named_object, location);
    case Named_object::NAMED_OBJECT_SINK:
      return Expression::make_sink(location);
    case Named_object::NAMED_OBJECT_FUNC:
    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      return Expression::make_func_reference(named_object, NULL, location);
    case Named_object::NAMED_OBJECT_UNKNOWN:
      {
	Unknown_expression* ue =
	  Expression::make_unknown_reference(named_object, location);
	if (this->is_erroneous_function_)
	  ue->set_no_error_message();
	return ue;
      }
    case Named_object::NAMED_OBJECT_PACKAGE:
    case Named_object::NAMED_OBJECT_TYPE:
    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
      {
	// These cases can arise for a field name in a composite
	// literal.  Keep track of these as they might be fake uses of
	// the related package.
	Unknown_expression* ue =
	  Expression::make_unknown_reference(named_object, location);
	if (named_object->package() != NULL)
	  named_object->package()->note_fake_usage(ue);
	if (this->is_erroneous_function_)
	  ue->set_no_error_message();
	return ue;
      }
    case Named_object::NAMED_OBJECT_ERRONEOUS:
      return Expression::make_error(location);
    default:
      go_error_at(this->location(), "unexpected type of identifier");
      return Expression::make_error(location);
    }
}

// Expression = UnaryExpr { binary_op Expression } .

// PRECEDENCE is the precedence of the current operator.

// If MAY_BE_SINK is true, this expression may be "_".

// If MAY_BE_COMPOSITE_LIT is true, this expression may be a composite
// literal.

// If IS_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

// If IS_PARENTHESIZED is not NULL, *IS_PARENTHESIZED is set to true
// if the entire expression is in parentheses.

Expression*
Parse::expression(Precedence precedence, bool may_be_sink,
		  bool may_be_composite_lit, bool* is_type_switch,
		  bool *is_parenthesized)
{
  Expression* left = this->unary_expr(may_be_sink, may_be_composite_lit,
				      is_type_switch, is_parenthesized);

  while (true)
    {
      if (is_type_switch != NULL && *is_type_switch)
	return left;

      const Token* token = this->peek_token();
      if (token->classification() != Token::TOKEN_OPERATOR)
	{
	  // Not a binary_op.
	  return left;
	}

      Precedence right_precedence;
      switch (token->op())
	{
	case OPERATOR_OROR:
	  right_precedence = PRECEDENCE_OROR;
	  break;
	case OPERATOR_ANDAND:
	  right_precedence = PRECEDENCE_ANDAND;
	  break;
	case OPERATOR_EQEQ:
	case OPERATOR_NOTEQ:
	case OPERATOR_LT:
	case OPERATOR_LE:
	case OPERATOR_GT:
	case OPERATOR_GE:
	  right_precedence = PRECEDENCE_RELOP;
	  break;
	case OPERATOR_PLUS:
	case OPERATOR_MINUS:
	case OPERATOR_OR:
	case OPERATOR_XOR:
	  right_precedence = PRECEDENCE_ADDOP;
	  break;
	case OPERATOR_MULT:
	case OPERATOR_DIV:
	case OPERATOR_MOD:
	case OPERATOR_LSHIFT:
	case OPERATOR_RSHIFT:
	case OPERATOR_AND:
	case OPERATOR_BITCLEAR:
	  right_precedence = PRECEDENCE_MULOP;
	  break;
	default:
	  right_precedence = PRECEDENCE_INVALID;
	  break;
	}

      if (right_precedence == PRECEDENCE_INVALID)
	{
	  // Not a binary_op.
	  return left;
	}

      if (is_parenthesized != NULL)
	*is_parenthesized = false;

      Operator op = token->op();
      Location binop_location = token->location();

      if (precedence >= right_precedence)
	{
	  // We've already seen A * B, and we see + C.  We want to
	  // return so that A * B becomes a group.
	  return left;
	}

      this->advance_token();

      left = this->verify_not_sink(left);
      Expression* right = this->expression(right_precedence, false,
					   may_be_composite_lit,
					   NULL, NULL);
      left = Expression::make_binary(op, left, right, binop_location);
    }
}

bool
Parse::expression_may_start_here()
{
  const Token* token = this->peek_token();
  switch (token->classification())
    {
    case Token::TOKEN_INVALID:
    case Token::TOKEN_EOF:
      return false;
    case Token::TOKEN_KEYWORD:
      switch (token->keyword())
	{
	case KEYWORD_CHAN:
	case KEYWORD_FUNC:
	case KEYWORD_MAP:
	case KEYWORD_STRUCT:
	case KEYWORD_INTERFACE:
	  return true;
	default:
	  return false;
	}
    case Token::TOKEN_IDENTIFIER:
      return true;
    case Token::TOKEN_STRING:
      return true;
    case Token::TOKEN_OPERATOR:
      switch (token->op())
	{
	case OPERATOR_PLUS:
	case OPERATOR_MINUS:
	case OPERATOR_NOT:
	case OPERATOR_XOR:
	case OPERATOR_MULT:
	case OPERATOR_CHANOP:
	case OPERATOR_AND:
	case OPERATOR_LPAREN:
	case OPERATOR_LSQUARE:
	  return true;
	default:
	  return false;
	}
    case Token::TOKEN_CHARACTER:
    case Token::TOKEN_INTEGER:
    case Token::TOKEN_FLOAT:
    case Token::TOKEN_IMAGINARY:
      return true;
    default:
      go_unreachable();
    }
}

// UnaryExpr = unary_op UnaryExpr | PrimaryExpr .

// If MAY_BE_SINK is true, this expression may be "_".

// If MAY_BE_COMPOSITE_LIT is true, this expression may be a composite
// literal.

// If IS_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

// If IS_PARENTHESIZED is not NULL, *IS_PARENTHESIZED is set to true
// if the entire expression is in parentheses.

Expression*
Parse::unary_expr(bool may_be_sink, bool may_be_composite_lit,
		  bool* is_type_switch, bool* is_parenthesized)
{
  const Token* token = this->peek_token();

  // There is a complex parse for <- chan.  The choices are
  // Convert x to type <- chan int:
  //   (<- chan int)(x)
  // Receive from (x converted to type chan <- chan int):
  //   (<- chan <- chan int (x))
  // Convert x to type <- chan (<- chan int).
  //   (<- chan <- chan int)(x)
  if (token->is_op(OPERATOR_CHANOP))
    {
      Location location = token->location();
      if (this->advance_token()->is_keyword(KEYWORD_CHAN))
	{
	  Expression* expr = this->primary_expr(false, may_be_composite_lit,
						NULL, NULL);
	  if (expr->is_error_expression())
	    return expr;
	  else if (!expr->is_type_expression())
	    return Expression::make_receive(expr, location);
	  else
	    {
	      if (expr->type()->is_error_type())
		return expr;

	      // We picked up "chan TYPE", but it is not a type
	      // conversion.
	      Channel_type* ct = expr->type()->channel_type();
	      if (ct == NULL)
		{
		  // This is probably impossible.
		  go_error_at(location, "expected channel type");
		  return Expression::make_error(location);
		}
	      else if (ct->may_receive())
		{
		  // <- chan TYPE.
		  Type* t = Type::make_channel_type(false, true,
						    ct->element_type());
		  return Expression::make_type(t, location);
		}
	      else
		{
		  // <- chan <- TYPE.  Because we skipped the leading
		  // <-, we parsed this as chan <- TYPE.  With the
		  // leading <-, we parse it as <- chan (<- TYPE).
		  Type *t = this->reassociate_chan_direction(ct, location);
		  return Expression::make_type(t, location);
		}
	    }
	}

      this->unget_token(Token::make_operator_token(OPERATOR_CHANOP, location));
      token = this->peek_token();
    }

  if (token->is_op(OPERATOR_PLUS)
      || token->is_op(OPERATOR_MINUS)
      || token->is_op(OPERATOR_NOT)
      || token->is_op(OPERATOR_XOR)
      || token->is_op(OPERATOR_CHANOP)
      || token->is_op(OPERATOR_MULT)
      || token->is_op(OPERATOR_AND))
    {
      Location location = token->location();
      Operator op = token->op();
      this->advance_token();

      Expression* expr = this->unary_expr(false, may_be_composite_lit, NULL,
					  NULL);
      if (expr->is_error_expression())
	;
      else if (op == OPERATOR_MULT && expr->is_type_expression())
	expr = Expression::make_type(Type::make_pointer_type(expr->type()),
				     location);
      else if (op == OPERATOR_AND && expr->is_composite_literal())
	expr = Expression::make_heap_expression(expr, location);
      else if (op != OPERATOR_CHANOP)
	expr = Expression::make_unary(op, expr, location);
      else
	expr = Expression::make_receive(expr, location);
      return expr;
    }
  else
    return this->primary_expr(may_be_sink, may_be_composite_lit,
			      is_type_switch, is_parenthesized);
}

// This is called for the obscure case of
//   (<- chan <- chan int)(x)
// In unary_expr we remove the leading <- and parse the remainder,
// which gives us
//   chan <- (chan int)
// When we add the leading <- back in, we really want
//   <- chan (<- chan int)
// This means that we need to reassociate.

Type*
Parse::reassociate_chan_direction(Channel_type *ct, Location location)
{
  Channel_type* ele = ct->element_type()->channel_type();
  if (ele == NULL)
    {
      go_error_at(location, "parse error");
      return Type::make_error_type();
    }
  Type* sub = ele;
  if (ele->may_send())
    sub = Type::make_channel_type(false, true, ele->element_type());
  else
    sub = this->reassociate_chan_direction(ele, location);
  return Type::make_channel_type(false, true, sub);
}

// Statement =
//	Declaration | LabeledStmt | SimpleStmt |
//	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
//	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
//	DeferStmt .

// LABEL is the label of this statement if it has one.

void
Parse::statement(Label* label)
{
  const Token* token = this->peek_token();
  switch (token->classification())
    {
    case Token::TOKEN_KEYWORD:
      {
	switch (token->keyword())
	  {
	  case KEYWORD_CONST:
	  case KEYWORD_TYPE:
	  case KEYWORD_VAR:
	    this->declaration();
	    break;
	  case KEYWORD_FUNC:
	  case KEYWORD_MAP:
	  case KEYWORD_STRUCT:
	  case KEYWORD_INTERFACE:
	    this->simple_stat(true, NULL, NULL, NULL);
	    break;
	  case KEYWORD_GO:
	  case KEYWORD_DEFER:
	    this->go_or_defer_stat();
	    break;
	  case KEYWORD_RETURN:
	    this->return_stat();
	    break;
	  case KEYWORD_BREAK:
	    this->break_stat();
	    break;
	  case KEYWORD_CONTINUE:
	    this->continue_stat();
	    break;
	  case KEYWORD_GOTO:
	    this->goto_stat();
	    break;
	  case KEYWORD_IF:
	    this->if_stat();
	    break;
	  case KEYWORD_SWITCH:
	    this->switch_stat(label);
	    break;
	  case KEYWORD_SELECT:
	    this->select_stat(label);
	    break;
	  case KEYWORD_FOR:
	    this->for_stat(label);
	    break;
	  default:
	    go_error_at(this->location(), "expected statement");
	    this->advance_token();
	    break;
	  }
      }
      break;

    case Token::TOKEN_IDENTIFIER:
      {
	std::string identifier = token->identifier();
	bool is_exported = token->is_identifier_exported();
	Location location = token->location();
	if (this->advance_token()->is_op(OPERATOR_COLON))
	  {
	    this->advance_token();
	    this->labeled_stmt(identifier, location);
	  }
	else
	  {
	    this->unget_token(Token::make_identifier_token(identifier,
							   is_exported,
							   location));
	    this->simple_stat(true, NULL, NULL, NULL);
	  }
      }
      break;

    case Token::TOKEN_OPERATOR:
      if (token->is_op(OPERATOR_LCURLY))
	{
	  Location location = token->location();
	  this->gogo_->start_block(location);
	  Location end_loc = this->block();
	  this->gogo_->add_block(this->gogo_->finish_block(end_loc),
				 location);
	}
      else if (!token->is_op(OPERATOR_SEMICOLON))
	this->simple_stat(true, NULL, NULL, NULL);
      break;

    case Token::TOKEN_STRING:
    case Token::TOKEN_CHARACTER:
    case Token::TOKEN_INTEGER:
    case Token::TOKEN_FLOAT:
    case Token::TOKEN_IMAGINARY:
      this->simple_stat(true, NULL, NULL, NULL);
      break;

    default:
      go_error_at(this->location(), "expected statement");
      this->advance_token();
      break;
    }
}

bool
Parse::statement_may_start_here()
{
  const Token* token = this->peek_token();
  switch (token->classification())
    {
    case Token::TOKEN_KEYWORD:
      {
	switch (token->keyword())
	  {
	  case KEYWORD_CONST:
	  case KEYWORD_TYPE:
	  case KEYWORD_VAR:
	  case KEYWORD_FUNC:
	  case KEYWORD_MAP:
	  case KEYWORD_STRUCT:
	  case KEYWORD_INTERFACE:
	  case KEYWORD_GO:
	  case KEYWORD_DEFER:
	  case KEYWORD_RETURN:
	  case KEYWORD_BREAK:
	  case KEYWORD_CONTINUE:
	  case KEYWORD_GOTO:
	  case KEYWORD_IF:
	  case KEYWORD_SWITCH:
	  case KEYWORD_SELECT:
	  case KEYWORD_FOR:
	    return true;

	  default:
	    return false;
	  }
      }
      break;

    case Token::TOKEN_IDENTIFIER:
      return true;

    case Token::TOKEN_OPERATOR:
      if (token->is_op(OPERATOR_LCURLY)
	  || token->is_op(OPERATOR_SEMICOLON))
	return true;
      else
	return this->expression_may_start_here();

    case Token::TOKEN_STRING:
    case Token::TOKEN_CHARACTER:
    case Token::TOKEN_INTEGER:
    case Token::TOKEN_FLOAT:
    case Token::TOKEN_IMAGINARY:
      return true;

    default:
      return false;
    }
}

// LabeledStmt = Label ":" Statement .
// Label       = identifier .

void
Parse::labeled_stmt(const std::string& label_name, Location location)
{
  Label* label = this->gogo_->add_label_definition(label_name, location);

  if (this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      // This is a label at the end of a block.  A program is
      // permitted to omit a semicolon here.
      return;
    }

  if (!this->statement_may_start_here())
    {
      if (this->peek_token()->is_keyword(KEYWORD_FALLTHROUGH))
	{
	  // We don't treat the fallthrough keyword as a statement,
	  // because it can't appear most places where a statement is
	  // permitted, but it may have a label.  We introduce a
	  // semicolon because the caller expects to see a statement.
	  this->unget_token(Token::make_operator_token(OPERATOR_SEMICOLON,
						       location));
	  return;
	}

      // Mark the label as used to avoid a useless error about an
      // unused label.
      if (label != NULL)
        label->set_is_used();

      go_error_at(location, "missing statement after label");
      this->unget_token(Token::make_operator_token(OPERATOR_SEMICOLON,
						   location));
      return;
    }

  this->statement(label);
}

// SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt |
//	Assignment | ShortVarDecl .

// EmptyStmt was handled in Parse::statement.

// In order to make this work for if and switch statements, if
// RETURN_EXP is not NULL, and we see an ExpressionStat, we return the
// expression rather than adding an expression statement to the
// current block.  If we see something other than an ExpressionStat,
// we add the statement, set *RETURN_EXP to true if we saw a send
// statement, and return NULL.  The handling of send statements is for
// better error messages.

// If P_RANGE_CLAUSE is not NULL, then this will recognize a
// RangeClause.

// If P_TYPE_SWITCH is not NULL, this will recognize a type switch
// guard (var := expr.("type") using the literal keyword "type").

Expression*
Parse::simple_stat(bool may_be_composite_lit, bool* return_exp,
		   Range_clause* p_range_clause, Type_switch* p_type_switch)
{
  const Token* token = this->peek_token();

  // An identifier follow by := is a SimpleVarDecl.
  if (token->is_identifier())
    {
      std::string identifier = token->identifier();
      bool is_exported = token->is_identifier_exported();
      Location location = token->location();

      token = this->advance_token();
      if (token->is_op(OPERATOR_COLONEQ)
	  || token->is_op(OPERATOR_COMMA))
	{
	  identifier = this->gogo_->pack_hidden_name(identifier, is_exported);
	  this->simple_var_decl_or_assignment(identifier, location,
					      may_be_composite_lit,
					      p_range_clause,
					      (token->is_op(OPERATOR_COLONEQ)
					       ? p_type_switch
					       : NULL));
	  return NULL;
	}

      this->unget_token(Token::make_identifier_token(identifier, is_exported,
						     location));
    }
  else if (p_range_clause != NULL && token->is_keyword(KEYWORD_RANGE))
    {
      Typed_identifier_list til;
      this->range_clause_decl(&til, p_range_clause);
      return NULL;
    }

  Expression* exp = this->expression(PRECEDENCE_NORMAL, true,
				     may_be_composite_lit,
				     (p_type_switch == NULL
				      ? NULL
				      : &p_type_switch->found),
				     NULL);
  if (p_type_switch != NULL && p_type_switch->found)
    {
      p_type_switch->name.clear();
      p_type_switch->location = exp->location();
      p_type_switch->expr = this->verify_not_sink(exp);
      return NULL;
    }
  token = this->peek_token();
  if (token->is_op(OPERATOR_CHANOP))
    {
      this->send_stmt(this->verify_not_sink(exp), may_be_composite_lit);
      if (return_exp != NULL)
	*return_exp = true;
    }
  else if (token->is_op(OPERATOR_PLUSPLUS)
	   || token->is_op(OPERATOR_MINUSMINUS))
    this->inc_dec_stat(this->verify_not_sink(exp));
  else if (token->is_op(OPERATOR_COMMA)
	   || token->is_op(OPERATOR_EQ))
    this->assignment(exp, may_be_composite_lit, p_range_clause);
  else if (token->is_op(OPERATOR_PLUSEQ)
	   || token->is_op(OPERATOR_MINUSEQ)
	   || token->is_op(OPERATOR_OREQ)
	   || token->is_op(OPERATOR_XOREQ)
	   || token->is_op(OPERATOR_MULTEQ)
	   || token->is_op(OPERATOR_DIVEQ)
	   || token->is_op(OPERATOR_MODEQ)
	   || token->is_op(OPERATOR_LSHIFTEQ)
	   || token->is_op(OPERATOR_RSHIFTEQ)
	   || token->is_op(OPERATOR_ANDEQ)
	   || token->is_op(OPERATOR_BITCLEAREQ))
    this->assignment(this->verify_not_sink(exp), may_be_composite_lit,
		     p_range_clause);
  else if (return_exp != NULL)
    return this->verify_not_sink(exp);
  else
    {
      exp = this->verify_not_sink(exp);

      if (token->is_op(OPERATOR_COLONEQ))
	{
	  if (!exp->is_error_expression())
	    go_error_at(token->location(), "non-name on left side of %<:=%>");
	  this->gogo_->mark_locals_used();
	  while (!token->is_op(OPERATOR_SEMICOLON)
		 && !token->is_eof())
	    token = this->advance_token();
	  return NULL;
	}

      this->expression_stat(exp);
    }

  return NULL;
}

bool
Parse::simple_stat_may_start_here()
{
  return this->expression_may_start_here();
}

// Parse { Statement ";" } which is used in a few places.  The list of
// statements may end with a right curly brace, in which case the
// semicolon may be omitted.

void
Parse::statement_list()
{
  while (this->statement_may_start_here())
    {
      this->statement(NULL);
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	this->advance_token();
      else if (this->peek_token()->is_op(OPERATOR_RCURLY))
	break;
      else
	{
	  if (!this->peek_token()->is_eof() || !saw_errors())
	    go_error_at(this->location(), "expected %<;%> or %<}%> or newline");
	  if (!this->skip_past_error(OPERATOR_RCURLY))
	    return;
	}
    }
}

bool
Parse::statement_list_may_start_here()
{
  return this->statement_may_start_here();
}

// ExpressionStat = Expression .

void
Parse::expression_stat(Expression* exp)
{
  this->gogo_->add_statement(Statement::make_statement(exp, false));
}

// SendStmt = Channel "&lt;-" Expression .
// Channel  = Expression .

void
Parse::send_stmt(Expression* channel, bool may_be_composite_lit)
{
  go_assert(this->peek_token()->is_op(OPERATOR_CHANOP));
  Location loc = this->location();
  this->advance_token();
  Expression* val = this->expression(PRECEDENCE_NORMAL, false,
				     may_be_composite_lit, NULL, NULL);
  Statement* s = Statement::make_send_statement(channel, val, loc);
  this->gogo_->add_statement(s);
}

// IncDecStat = Expression ( "++" | "--" ) .

void
Parse::inc_dec_stat(Expression* exp)
{
  const Token* token = this->peek_token();
  if (token->is_op(OPERATOR_PLUSPLUS))
    this->gogo_->add_statement(Statement::make_inc_statement(exp));
  else if (token->is_op(OPERATOR_MINUSMINUS))
    this->gogo_->add_statement(Statement::make_dec_statement(exp));
  else
    go_unreachable();
  this->advance_token();
}

// Assignment = ExpressionList assign_op ExpressionList .

// EXP is an expression that we have already parsed.

// If MAY_BE_COMPOSITE_LIT is true, an expression on the right hand
// side may be a composite literal.

// If RANGE_CLAUSE is not NULL, then this will recognize a
// RangeClause.

void
Parse::assignment(Expression* expr, bool may_be_composite_lit,
		  Range_clause* p_range_clause)
{
  Expression_list* vars;
  if (!this->peek_token()->is_op(OPERATOR_COMMA))
    {
      vars = new Expression_list();
      vars->push_back(expr);
    }
  else
    {
      this->advance_token();
      vars = this->expression_list(expr, true, may_be_composite_lit);
    }

  this->tuple_assignment(vars, may_be_composite_lit, p_range_clause);
}

// An assignment statement.  LHS is the list of expressions which
// appear on the left hand side.

// If MAY_BE_COMPOSITE_LIT is true, an expression on the right hand
// side may be a composite literal.

// If RANGE_CLAUSE is not NULL, then this will recognize a
// RangeClause.

void
Parse::tuple_assignment(Expression_list* lhs, bool may_be_composite_lit,
			Range_clause* p_range_clause)
{
  const Token* token = this->peek_token();
  if (!token->is_op(OPERATOR_EQ)
      && !token->is_op(OPERATOR_PLUSEQ)
      && !token->is_op(OPERATOR_MINUSEQ)
      && !token->is_op(OPERATOR_OREQ)
      && !token->is_op(OPERATOR_XOREQ)
      && !token->is_op(OPERATOR_MULTEQ)
      && !token->is_op(OPERATOR_DIVEQ)
      && !token->is_op(OPERATOR_MODEQ)
      && !token->is_op(OPERATOR_LSHIFTEQ)
      && !token->is_op(OPERATOR_RSHIFTEQ)
      && !token->is_op(OPERATOR_ANDEQ)
      && !token->is_op(OPERATOR_BITCLEAREQ))
    {
      go_error_at(this->location(), "expected assignment operator");
      return;
    }
  Operator op = token->op();
  Location location = token->location();

  token = this->advance_token();

  if (lhs == NULL)
    return;

  if (p_range_clause != NULL && token->is_keyword(KEYWORD_RANGE))
    {
      if (op != OPERATOR_EQ)
	go_error_at(this->location(), "range clause requires %<=%>");
      this->range_clause_expr(lhs, p_range_clause);
      return;
    }

  Expression_list* vals = this->expression_list(NULL, false,
						may_be_composite_lit);

  // We've parsed everything; check for errors.
  if (vals == NULL)
    return;
  for (Expression_list::const_iterator pe = lhs->begin();
       pe != lhs->end();
       ++pe)
    {
      if ((*pe)->is_error_expression())
	return;
      if (op != OPERATOR_EQ && (*pe)->is_sink_expression())
	go_error_at((*pe)->location(), "cannot use %<_%> as value");
    }
  for (Expression_list::const_iterator pe = vals->begin();
       pe != vals->end();
       ++pe)
    {
      if ((*pe)->is_error_expression())
	return;
    }

  Call_expression* call;
  Index_expression* map_index;
  Receive_expression* receive;
  Type_guard_expression* type_guard;
  if (lhs->size() == vals->size())
    {
      Statement* s;
      if (lhs->size() > 1)
	{
	  if (op != OPERATOR_EQ)
	    go_error_at(location, "multiple values only permitted with %<=%>");
	  s = Statement::make_tuple_assignment(lhs, vals, location);
	}
      else
	{
	  if (op == OPERATOR_EQ)
	    s = Statement::make_assignment(lhs->front(), vals->front(),
					   location);
	  else
	    s = Statement::make_assignment_operation(op, lhs->front(),
						     vals->front(), location);
	  delete lhs;
	  delete vals;
	}
      this->gogo_->add_statement(s);
    }
  else if (vals->size() == 1
	   && (call = (*vals->begin())->call_expression()) != NULL)
    {
      if (op != OPERATOR_EQ)
	go_error_at(location, "multiple results only permitted with %<=%>");
      call->set_expected_result_count(lhs->size());
      delete vals;
      vals = new Expression_list;
      for (unsigned int i = 0; i < lhs->size(); ++i)
	vals->push_back(Expression::make_call_result(call, i));
      Statement* s = Statement::make_tuple_assignment(lhs, vals, location);
      this->gogo_->add_statement(s);
    }
  else if (lhs->size() == 2
	   && vals->size() == 1
	   && (map_index = (*vals->begin())->index_expression()) != NULL)
    {
      if (op != OPERATOR_EQ)
	go_error_at(location, "two values from map requires %<=%>");
      Expression* val = lhs->front();
      Expression* present = lhs->back();
      Statement* s = Statement::make_tuple_map_assignment(val, present,
							  map_index, location);
      this->gogo_->add_statement(s);
    }
  else if (lhs->size() == 2
	   && vals->size() == 1
	   && (receive = (*vals->begin())->receive_expression()) != NULL)
    {
      if (op != OPERATOR_EQ)
	go_error_at(location, "two values from receive requires %<=%>");
      Expression* val = lhs->front();
      Expression* success = lhs->back();
      Expression* channel = receive->channel();
      Statement* s = Statement::make_tuple_receive_assignment(val, success,
							      channel,
							      location);
      this->gogo_->add_statement(s);
    }
  else if (lhs->size() == 2
	   && vals->size() == 1
	   && (type_guard = (*vals->begin())->type_guard_expression()) != NULL)
    {
      if (op != OPERATOR_EQ)
	go_error_at(location, "two values from type guard requires %<=%>");
      Expression* val = lhs->front();
      Expression* ok = lhs->back();
      Expression* expr = type_guard->expr();
      Type* type = type_guard->type();
      Statement* s = Statement::make_tuple_type_guard_assignment(val, ok,
								 expr, type,
								 location);
      this->gogo_->add_statement(s);
    }
  else
    {
      go_error_at(location, ("number of variables does not "
                             "match number of values"));
    }
}

// GoStat = "go" Expression .
// DeferStat = "defer" Expression .

void
Parse::go_or_defer_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_GO)
	     || this->peek_token()->is_keyword(KEYWORD_DEFER));
  bool is_go = this->peek_token()->is_keyword(KEYWORD_GO);
  Location stat_location = this->location();

  this->advance_token();
  Location expr_location = this->location();

  bool is_parenthesized = false;
  Expression* expr = this->expression(PRECEDENCE_NORMAL, false, true, NULL,
				      &is_parenthesized);
  Call_expression* call_expr = expr->call_expression();
  if (is_parenthesized || call_expr == NULL)
    {
      go_error_at(expr_location, "argument to go/defer must be function call");
      return;
    }

  // Make it easier to simplify go/defer statements by putting every
  // statement in its own block.
  this->gogo_->start_block(stat_location);
  Statement* stat;
  if (is_go)
    {
      stat = Statement::make_go_statement(call_expr, stat_location);
      call_expr->set_is_concurrent();
    }
  else
    {
      stat = Statement::make_defer_statement(call_expr, stat_location);
      call_expr->set_is_deferred();
    }
  this->gogo_->add_statement(stat);
  this->gogo_->add_block(this->gogo_->finish_block(stat_location),
			 stat_location);
}

// ReturnStat = "return" [ ExpressionList ] .

void
Parse::return_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_RETURN));
  Location location = this->location();
  this->advance_token();
  Expression_list* vals = NULL;
  if (this->expression_may_start_here())
    vals = this->expression_list(NULL, false, true);
  Named_object* function = this->gogo_->current_function();
  this->gogo_->add_statement(Statement::make_return_statement(function, vals,
							      location));

  if (vals == NULL && function->func_value()->results_are_named())
    {
      Function::Results* results = function->func_value()->result_variables();
      for (Function::Results::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
	{
	  Named_object* no = this->gogo_->lookup((*p)->name(), NULL);
	  if (no == NULL)
	    go_assert(saw_errors());
	  else if (!no->is_result_variable())
	    go_error_at(location, "%qs is shadowed during return",
			(*p)->message_name().c_str());
	}
    }
}

// IfStmt = "if" [ SimpleStmt ";" ] Expression Block
//          [ "else" ( IfStmt | Block ) ] .

void
Parse::if_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_IF));
  Location location = this->location();
  this->advance_token();

  this->gogo_->start_block(location);

  bool saw_simple_stat = false;
  Expression* cond = NULL;
  bool saw_send_stmt = false;
  if (this->simple_stat_may_start_here())
    {
      cond = this->simple_stat(false, &saw_send_stmt, NULL, NULL);
      saw_simple_stat = true;
    }
  if (cond != NULL && this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      // The SimpleStat is an expression statement.
      this->expression_stat(cond);
      cond = NULL;
    }
  if (cond == NULL)
    {
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	this->advance_token();
      else if (saw_simple_stat)
	{
	  if (saw_send_stmt)
	    go_error_at(this->location(),
			("send statement used as value; "
			 "use select for non-blocking send"));
	  else
	    go_error_at(this->location(),
			"expected %<;%> after statement in if expression");
	  if (!this->expression_may_start_here())
	    cond = Expression::make_error(this->location());
	}
      if (cond == NULL && this->peek_token()->is_op(OPERATOR_LCURLY))
	{
	  go_error_at(this->location(),
		      "missing condition in if statement");
	  cond = Expression::make_error(this->location());
	}
      if (cond == NULL)
	cond = this->expression(PRECEDENCE_NORMAL, false, false, NULL, NULL);
    }

  // Check for the easy error of a newline before starting the block.
  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      Location semi_loc = this->location();
      if (this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(semi_loc, "unexpected semicolon or newline, expecting %<{%> after if clause");
      // Otherwise we will get an error when we call this->block
      // below.
    }

  this->gogo_->start_block(this->location());
  Location end_loc = this->block();
  Block* then_block = this->gogo_->finish_block(end_loc);

  // Check for the easy error of a newline before "else".
  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      Location semi_loc = this->location();
      if (this->advance_token()->is_keyword(KEYWORD_ELSE))
	go_error_at(this->location(),
		 "unexpected semicolon or newline before %<else%>");
      else
	this->unget_token(Token::make_operator_token(OPERATOR_SEMICOLON,
						     semi_loc));
    }

  Block* else_block = NULL;
  if (this->peek_token()->is_keyword(KEYWORD_ELSE))
    {
      this->gogo_->start_block(this->location());
      const Token* token = this->advance_token();
      if (token->is_keyword(KEYWORD_IF))
	this->if_stat();
      else if (token->is_op(OPERATOR_LCURLY))
	this->block();
      else
	{
	  go_error_at(this->location(), "expected %<if%> or %<{%>");
	  this->statement(NULL);
	}
      else_block = this->gogo_->finish_block(this->location());
    }

  this->gogo_->add_statement(Statement::make_if_statement(cond, then_block,
							  else_block,
							  location));

  this->gogo_->add_block(this->gogo_->finish_block(this->location()),
			 location);
}

// SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
// ExprSwitchStmt = "switch" [ [ SimpleStat ] ";" ] [ Expression ]
//			"{" { ExprCaseClause } "}" .
// TypeSwitchStmt  = "switch" [ [ SimpleStat ] ";" ] TypeSwitchGuard
//			"{" { TypeCaseClause } "}" .
// TypeSwitchGuard = [ identifier ":=" ] Expression "." "(" "type" ")" .

void
Parse::switch_stat(Label* label)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_SWITCH));
  Location location = this->location();
  this->advance_token();

  this->gogo_->start_block(location);

  bool saw_simple_stat = false;
  Expression* switch_val = NULL;
  bool saw_send_stmt = false;
  Type_switch type_switch;
  bool have_type_switch_block = false;
  if (this->simple_stat_may_start_here())
    {
      switch_val = this->simple_stat(false, &saw_send_stmt, NULL,
				     &type_switch);
      saw_simple_stat = true;
    }
  if (switch_val != NULL && this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      // The SimpleStat is an expression statement.
      this->expression_stat(switch_val);
      switch_val = NULL;
    }
  if (switch_val == NULL && !type_switch.found)
    {
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	this->advance_token();
      else if (saw_simple_stat)
	{
	  if (saw_send_stmt)
	    go_error_at(this->location(),
			("send statement used as value; "
			 "use select for non-blocking send"));
	  else
	    go_error_at(this->location(),
			"expected %<;%> after statement in switch expression");
	}
      if (!this->peek_token()->is_op(OPERATOR_LCURLY))
	{
	  if (this->peek_token()->is_identifier())
	    {
	      const Token* token = this->peek_token();
	      std::string identifier = token->identifier();
	      bool is_exported = token->is_identifier_exported();
	      Location id_loc = token->location();

	      token = this->advance_token();
	      bool is_coloneq = token->is_op(OPERATOR_COLONEQ);
	      this->unget_token(Token::make_identifier_token(identifier,
							     is_exported,
							     id_loc));
	      if (is_coloneq)
		{
		  // This must be a TypeSwitchGuard.  It is in a
		  // different block from any initial SimpleStat.
		  if (saw_simple_stat)
		    {
		      this->gogo_->start_block(id_loc);
		      have_type_switch_block = true;
		    }

		  switch_val = this->simple_stat(false, &saw_send_stmt, NULL,
						 &type_switch);
		  if (!type_switch.found)
		    {
		      if (switch_val == NULL
			  || !switch_val->is_error_expression())
			{
			  go_error_at(id_loc,
				      "expected type switch assignment");
			  switch_val = Expression::make_error(id_loc);
			}
		    }
		}
	    }
	  if (switch_val == NULL && !type_switch.found)
	    {
	      switch_val = this->expression(PRECEDENCE_NORMAL, false, false,
					    &type_switch.found, NULL);
	      if (type_switch.found)
		{
		  type_switch.name.clear();
		  type_switch.expr = switch_val;
		  type_switch.location = switch_val->location();
		}
	    }
	}
    }

  if (!this->peek_token()->is_op(OPERATOR_LCURLY))
    {
      Location token_loc = this->location();
      if (this->peek_token()->is_op(OPERATOR_SEMICOLON)
	  && this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(token_loc, "missing %<{%> after switch clause");
      else if (this->peek_token()->is_op(OPERATOR_COLONEQ))
	{
	  go_error_at(token_loc, "invalid variable name");
	  this->advance_token();
	  this->expression(PRECEDENCE_NORMAL, false, false,
			   &type_switch.found, NULL);
	  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
	    this->advance_token();
	  if (!this->peek_token()->is_op(OPERATOR_LCURLY))
	    {
	      if (have_type_switch_block)
		this->gogo_->add_block(this->gogo_->finish_block(location),
				       location);
	      this->gogo_->add_block(this->gogo_->finish_block(location),
				     location);
	      return;
	    }
	  if (type_switch.found)
	    type_switch.expr = Expression::make_error(location);
	}
      else
	{
	  go_error_at(this->location(), "expected %<{%>");
	  if (have_type_switch_block)
	    this->gogo_->add_block(this->gogo_->finish_block(this->location()),
				   location);
	  this->gogo_->add_block(this->gogo_->finish_block(this->location()),
				 location);
	  return;
	}
    }
  this->advance_token();

  Statement* statement;
  if (type_switch.found)
    statement = this->type_switch_body(label, type_switch, location);
  else
    statement = this->expr_switch_body(label, switch_val, location);

  if (statement != NULL)
    this->gogo_->add_statement(statement);

  if (have_type_switch_block)
    this->gogo_->add_block(this->gogo_->finish_block(this->location()),
			   location);

  this->gogo_->add_block(this->gogo_->finish_block(this->location()),
			 location);
}

// The body of an expression switch.
//   "{" { ExprCaseClause } "}"

Statement*
Parse::expr_switch_body(Label* label, Expression* switch_val,
			Location location)
{
  Switch_statement* statement = Statement::make_switch_statement(switch_val,
								 location);

  this->push_break_statement(statement, label);

  Case_clauses* case_clauses = new Case_clauses();
  bool saw_default = false;
  while (!this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      if (this->peek_token()->is_eof())
	{
	  if (!saw_errors())
	    go_error_at(this->location(), "missing %<}%>");
	  return NULL;
	}
      this->expr_case_clause(case_clauses, &saw_default);
    }
  this->advance_token();

  statement->add_clauses(case_clauses);

  this->pop_break_statement();

  return statement;
}

// ExprCaseClause = ExprSwitchCase ":" [ StatementList ] .
// FallthroughStat = "fallthrough" .

void
Parse::expr_case_clause(Case_clauses* clauses, bool* saw_default)
{
  Location location = this->location();

  bool is_default = false;
  Expression_list* vals = this->expr_switch_case(&is_default);

  if (!this->peek_token()->is_op(OPERATOR_COLON))
    {
      if (!saw_errors())
	go_error_at(this->location(), "expected %<:%>");
      return;
    }
  else
    this->advance_token();

  Block* statements = NULL;
  if (this->statement_list_may_start_here())
    {
      this->gogo_->start_block(this->location());
      this->statement_list();
      statements = this->gogo_->finish_block(this->location());
    }

  bool is_fallthrough = false;
  if (this->peek_token()->is_keyword(KEYWORD_FALLTHROUGH))
    {
      Location fallthrough_loc = this->location();
      is_fallthrough = true;
      while (this->advance_token()->is_op(OPERATOR_SEMICOLON))
	;
      if (this->peek_token()->is_op(OPERATOR_RCURLY))
	go_error_at(fallthrough_loc,
		    _("cannot fallthrough final case in switch"));
      else if (!this->peek_token()->is_keyword(KEYWORD_CASE)
	       && !this->peek_token()->is_keyword(KEYWORD_DEFAULT))
	{
	  go_error_at(fallthrough_loc, "fallthrough statement out of place");
	  while (!this->peek_token()->is_keyword(KEYWORD_CASE)
		 && !this->peek_token()->is_keyword(KEYWORD_DEFAULT)
		 && !this->peek_token()->is_op(OPERATOR_RCURLY)
		 && !this->peek_token()->is_eof())
	    {
	      if (this->statement_may_start_here())
		this->statement_list();
	      else
		this->advance_token();
	    }
	}
    }

  if (is_default)
    {
      if (*saw_default)
	{
	  go_error_at(location, "multiple defaults in switch");
	  return;
	}
      *saw_default = true;
    }

  if (is_default || vals != NULL)
    clauses->add(vals, is_default, statements, is_fallthrough, location);
}

// ExprSwitchCase = "case" ExpressionList | "default" .

Expression_list*
Parse::expr_switch_case(bool* is_default)
{
  const Token* token = this->peek_token();
  if (token->is_keyword(KEYWORD_CASE))
    {
      this->advance_token();
      return this->expression_list(NULL, false, true);
    }
  else if (token->is_keyword(KEYWORD_DEFAULT))
    {
      this->advance_token();
      *is_default = true;
      return NULL;
    }
  else
    {
      if (!saw_errors())
	go_error_at(this->location(), "expected %<case%> or %<default%>");
      if (!token->is_op(OPERATOR_RCURLY))
	this->advance_token();
      return NULL;
    }
}

// The body of a type switch.
//   "{" { TypeCaseClause } "}" .

Statement*
Parse::type_switch_body(Label* label, const Type_switch& type_switch,
			Location location)
{
  Expression* init = type_switch.expr;
  std::string var_name = type_switch.name;
  if (!var_name.empty())
    {
      if (Gogo::is_sink_name(var_name))
        {
	  go_error_at(type_switch.location,
		      "no new variables on left side of %<:=%>");
          var_name.clear();
        }
      else
	{
          Location loc = type_switch.location;
	  Temporary_statement* switch_temp =
              Statement::make_temporary(NULL, init, loc);
	  this->gogo_->add_statement(switch_temp);
          init = Expression::make_temporary_reference(switch_temp, loc);
	}
    }

  Type_switch_statement* statement =
      Statement::make_type_switch_statement(init, location);
  this->push_break_statement(statement, label);

  Type_case_clauses* case_clauses = new Type_case_clauses();
  bool saw_default = false;
  std::vector<Named_object*> implicit_vars;
  while (!this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      if (this->peek_token()->is_eof())
	{
	  go_error_at(this->location(), "missing %<}%>");
	  return NULL;
	}
      this->type_case_clause(var_name, init, case_clauses, &saw_default,
                             &implicit_vars);
    }
  this->advance_token();

  statement->add_clauses(case_clauses);

  this->pop_break_statement();

  // If there is a type switch variable implicitly declared in each case clause,
  // check that it is used in at least one of the cases.
  if (!var_name.empty())
    {
      bool used = false;
      for (std::vector<Named_object*>::iterator p = implicit_vars.begin();
	   p != implicit_vars.end();
	   ++p)
	{
	  if ((*p)->var_value()->is_used())
	    {
	      used = true;
	      break;
	    }
	}
      if (!used)
	go_error_at(type_switch.location, "%qs declared but not used",
		    Gogo::message_name(var_name).c_str());
    }
  return statement;
}

// TypeCaseClause  = TypeSwitchCase ":" [ StatementList ] .
// IMPLICIT_VARS is the list of variables implicitly declared for each type
// case if there is a type switch variable declared.

void
Parse::type_case_clause(const std::string& var_name, Expression* init,
                        Type_case_clauses* clauses, bool* saw_default,
			std::vector<Named_object*>* implicit_vars)
{
  Location location = this->location();

  std::vector<Type*> types;
  bool is_default = false;
  this->type_switch_case(&types, &is_default);

  if (!this->peek_token()->is_op(OPERATOR_COLON))
    go_error_at(this->location(), "expected %<:%>");
  else
    this->advance_token();

  Block* statements = NULL;
  if (this->statement_list_may_start_here())
    {
      this->gogo_->start_block(this->location());
      if (!var_name.empty())
	{
	  Type* type = NULL;
          Location var_loc = init->location();
	  if (types.size() == 1)
	    {
	      type = types.front();
	      init = Expression::make_type_guard(init, type, location);
	    }

	  Variable* v = new Variable(type, init, false, false, false,
				     var_loc);
	  v->set_is_used();
	  v->set_is_type_switch_var();
	  implicit_vars->push_back(this->gogo_->add_variable(var_name, v));
	}
      this->statement_list();
      statements = this->gogo_->finish_block(this->location());
    }

  if (this->peek_token()->is_keyword(KEYWORD_FALLTHROUGH))
    {
      go_error_at(this->location(),
		  "fallthrough is not permitted in a type switch");
      if (this->advance_token()->is_op(OPERATOR_SEMICOLON))
	this->advance_token();
    }

  if (is_default)
    {
      go_assert(types.empty());
      if (*saw_default)
	{
	  go_error_at(location, "multiple defaults in type switch");
	  return;
	}
      *saw_default = true;
      clauses->add(NULL, false, true, statements, location);
    }
  else if (!types.empty())
    {
      for (std::vector<Type*>::const_iterator p = types.begin();
	   p + 1 != types.end();
	   ++p)
	clauses->add(*p, true, false, NULL, location);
      clauses->add(types.back(), false, false, statements, location);
    }
  else
    clauses->add(Type::make_error_type(), false, false, statements, location);
}

// TypeSwitchCase  = "case" type | "default"

// We accept a comma separated list of types.

void
Parse::type_switch_case(std::vector<Type*>* types, bool* is_default)
{
  const Token* token = this->peek_token();
  if (token->is_keyword(KEYWORD_CASE))
    {
      this->advance_token();
      while (true)
	{
	  Type* t = this->type();

	  if (!t->is_error_type())
	    types->push_back(t);
	  else
	    {
	      this->gogo_->mark_locals_used();
	      token = this->peek_token();
	      while (!token->is_op(OPERATOR_COLON)
		     && !token->is_op(OPERATOR_COMMA)
		     && !token->is_op(OPERATOR_RCURLY)
		     && !token->is_eof())
		token = this->advance_token();
	    }

	  if (!this->peek_token()->is_op(OPERATOR_COMMA))
	    break;
	  this->advance_token();
	}
    }
  else if (token->is_keyword(KEYWORD_DEFAULT))
    {
      this->advance_token();
      *is_default = true;
    }
  else
    {
      go_error_at(this->location(), "expected %<case%> or %<default%>");
      if (!token->is_op(OPERATOR_RCURLY))
	this->advance_token();
    }
}

// SelectStat = "select" "{" { CommClause } "}" .

void
Parse::select_stat(Label* label)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_SELECT));
  Location location = this->location();
  const Token* token = this->advance_token();

  if (!token->is_op(OPERATOR_LCURLY))
    {
      Location token_loc = token->location();
      if (token->is_op(OPERATOR_SEMICOLON)
	  && this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(token_loc, "unexpected semicolon or newline before %<{%>");
      else
	{
	  go_error_at(this->location(), "expected %<{%>");
	  return;
	}
    }
  this->advance_token();

  Select_statement* statement = Statement::make_select_statement(location);

  this->push_break_statement(statement, label);

  Select_clauses* select_clauses = new Select_clauses();
  bool saw_default = false;
  while (!this->peek_token()->is_op(OPERATOR_RCURLY))
    {
      if (this->peek_token()->is_eof())
	{
	  go_error_at(this->location(), "expected %<}%>");
	  return;
	}
      this->comm_clause(select_clauses, &saw_default);
    }

  this->advance_token();

  statement->add_clauses(select_clauses);

  this->pop_break_statement();

  this->gogo_->add_statement(statement);
}

// CommClause = CommCase ":" { Statement ";" } .

void
Parse::comm_clause(Select_clauses* clauses, bool* saw_default)
{
  Location location = this->location();
  bool is_send = false;
  Expression* channel = NULL;
  Expression* val = NULL;
  Expression* closed = NULL;
  std::string varname;
  std::string closedname;
  bool is_default = false;
  bool got_case = this->comm_case(&is_send, &channel, &val, &closed,
				  &varname, &closedname, &is_default);

  if (this->peek_token()->is_op(OPERATOR_COLON))
    this->advance_token();
  else
    go_error_at(this->location(), "expected colon");

  this->gogo_->start_block(this->location());

  Named_object* var = NULL;
  if (!varname.empty())
    {
      // FIXME: LOCATION is slightly wrong here.
      Variable* v = new Variable(NULL, channel, false, false, false,
				 location);
      v->set_type_from_chan_element();
      var = this->gogo_->add_variable(varname, v);
    }

  Named_object* closedvar = NULL;
  if (!closedname.empty())
    {
      // FIXME: LOCATION is slightly wrong here.
      Variable* v = new Variable(Type::lookup_bool_type(), NULL,
				 false, false, false, location);
      closedvar = this->gogo_->add_variable(closedname, v);
    }

  this->statement_list();

  Block* statements = this->gogo_->finish_block(this->location());

  if (is_default)
    {
      if (*saw_default)
	{
	  go_error_at(location, "multiple defaults in select");
	  return;
	}
      *saw_default = true;
    }

  if (got_case)
    clauses->add(is_send, channel, val, closed, var, closedvar, is_default,
		 statements, location);
  else if (statements != NULL)
    {
      // Add the statements to make sure that any names they define
      // are traversed.
      this->gogo_->add_block(statements, location);
    }
}

// CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .

bool
Parse::comm_case(bool* is_send, Expression** channel, Expression** val,
		 Expression** closed, std::string* varname,
		 std::string* closedname, bool* is_default)
{
  const Token* token = this->peek_token();
  if (token->is_keyword(KEYWORD_DEFAULT))
    {
      this->advance_token();
      *is_default = true;
    }
  else if (token->is_keyword(KEYWORD_CASE))
    {
      this->advance_token();
      if (!this->send_or_recv_stmt(is_send, channel, val, closed, varname,
				   closedname))
	return false;
    }
  else
    {
      go_error_at(this->location(), "expected %<case%> or %<default%>");
      if (!token->is_op(OPERATOR_RCURLY))
	this->advance_token();
      return false;
    }

  return true;
}

// RecvStmt   = [ Expression [ "," Expression ] ( "=" | ":=" ) ] RecvExpr .
// RecvExpr   = Expression .

bool
Parse::send_or_recv_stmt(bool* is_send, Expression** channel, Expression** val,
			 Expression** closed, std::string* varname,
			 std::string* closedname)
{
  const Token* token = this->peek_token();
  bool saw_comma = false;
  bool closed_is_id = false;
  if (token->is_identifier())
    {
      Gogo* gogo = this->gogo_;
      std::string recv_var = token->identifier();
      bool is_rv_exported = token->is_identifier_exported();
      Location recv_var_loc = token->location();
      token = this->advance_token();
      if (token->is_op(OPERATOR_COLONEQ))
	{
	  // case rv := <-c:
	  this->advance_token();
	  Expression* e = this->expression(PRECEDENCE_NORMAL, false, false,
					   NULL, NULL);
	  Receive_expression* re = e->receive_expression();
	  if (re == NULL)
	    {
	      if (!e->is_error_expression())
		go_error_at(this->location(), "expected receive expression");
	      return false;
	    }
	  if (recv_var == "_")
	    {
	      go_error_at(recv_var_loc,
			  "no new variables on left side of %<:=%>");
	      recv_var = Gogo::erroneous_name();
	    }
	  *is_send = false;
	  *varname = gogo->pack_hidden_name(recv_var, is_rv_exported);
	  *channel = re->channel();
	  return true;
	}
      else if (token->is_op(OPERATOR_COMMA))
	{
	  token = this->advance_token();
	  if (token->is_identifier())
	    {
	      std::string recv_closed = token->identifier();
	      bool is_rc_exported = token->is_identifier_exported();
	      Location recv_closed_loc = token->location();
	      closed_is_id = true;

	      token = this->advance_token();
	      if (token->is_op(OPERATOR_COLONEQ))
		{
		  // case rv, rc := <-c:
		  this->advance_token();
		  Expression* e = this->expression(PRECEDENCE_NORMAL, false,
						   false, NULL, NULL);
		  Receive_expression* re = e->receive_expression();
		  if (re == NULL)
		    {
		      if (!e->is_error_expression())
			go_error_at(this->location(),
				 "expected receive expression");
		      return false;
		    }
		  if (recv_var == "_" && recv_closed == "_")
		    {
		      go_error_at(recv_var_loc,
				  "no new variables on left side of %<:=%>");
		      recv_var = Gogo::erroneous_name();
		    }
		  *is_send = false;
		  if (recv_var != "_")
		    *varname = gogo->pack_hidden_name(recv_var,
						      is_rv_exported);
		  if (recv_closed != "_")
		    *closedname = gogo->pack_hidden_name(recv_closed,
							 is_rc_exported);
		  *channel = re->channel();
		  return true;
		}

	      this->unget_token(Token::make_identifier_token(recv_closed,
							     is_rc_exported,
							     recv_closed_loc));
	    }

	  *val = this->id_to_expression(gogo->pack_hidden_name(recv_var,
							       is_rv_exported),
					recv_var_loc, true, false);
	  saw_comma = true;
	}
      else
	this->unget_token(Token::make_identifier_token(recv_var,
						       is_rv_exported,
						       recv_var_loc));
    }

  // If SAW_COMMA is false, then we are looking at the start of the
  // send or receive expression.  If SAW_COMMA is true, then *VAL is
  // set and we just read a comma.

  Expression* e;
  if (saw_comma || !this->peek_token()->is_op(OPERATOR_CHANOP))
    {
      e = this->expression(PRECEDENCE_NORMAL, true, true, NULL, NULL);
      if (e->receive_expression() != NULL)
	{
	  *is_send = false;
	  *channel = e->receive_expression()->channel();
	  // This is 'case (<-c):'.  We now expect ':'.  If we see
	  // '<-', then we have case (<-c)<-v:
	  if (!this->peek_token()->is_op(OPERATOR_CHANOP))
	    return true;
	}
    }
  else
    {
      // case <-c:
      *is_send = false;
      this->advance_token();
      *channel = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);

      // The next token should be ':'.  If it is '<-', then we have
      // case <-c <- v:
      // which is to say, send on a channel received from a channel.
      if (!this->peek_token()->is_op(OPERATOR_CHANOP))
	return true;

      e = Expression::make_receive(*channel, (*channel)->location());
    }

  if (!saw_comma && this->peek_token()->is_op(OPERATOR_COMMA))
    {
      this->advance_token();
      // case v, e = <-c:
      if (!e->is_sink_expression())
	*val = e;
      e = this->expression(PRECEDENCE_NORMAL, true, true, NULL, NULL);
      saw_comma = true;
    }

  if (this->peek_token()->is_op(OPERATOR_EQ))
    {
      *is_send = false;
      this->advance_token();
      Location recvloc = this->location();
      Expression* recvexpr = this->expression(PRECEDENCE_NORMAL, false,
					      true, NULL, NULL);
      if (recvexpr->receive_expression() == NULL)
	{
	  go_error_at(recvloc, "missing %<<-%>");
	  return false;
	}
      *channel = recvexpr->receive_expression()->channel();
      if (saw_comma)
	{
	  // case v, e = <-c:
	  // *VAL is already set.
	  if (!e->is_sink_expression())
	    *closed = e;
	}
      else
	{
	  // case v = <-c:
	  if (!e->is_sink_expression())
	    *val = e;
	}
      return true;
    }

  if (saw_comma)
    {
      if (closed_is_id)
	go_error_at(this->location(), "expected %<=%> or %<:=%>");
      else
	go_error_at(this->location(), "expected %<=%>");
      return false;
    }

  if (this->peek_token()->is_op(OPERATOR_CHANOP))
    {
      // case c <- v:
      *is_send = true;
      *channel = this->verify_not_sink(e);
      this->advance_token();
      *val = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
      return true;
    }

  go_error_at(this->location(), "expected %<<-%> or %<=%>");
  return false;
}

// ForStat = "for" [ Condition | ForClause | RangeClause ] Block .
// Condition = Expression .

void
Parse::for_stat(Label* label)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_FOR));
  Location location = this->location();
  const Token* token = this->advance_token();

  // Open a block to hold any variables defined in the init statement
  // of the for statement.
  this->gogo_->start_block(location);

  Block* init = NULL;
  Expression* cond = NULL;
  Block* post = NULL;
  Range_clause range_clause;

  if (!token->is_op(OPERATOR_LCURLY))
    {
      if (token->is_keyword(KEYWORD_VAR))
	{
	  go_error_at(this->location(),
                      "var declaration not allowed in for initializer");
	  this->var_decl();
	}

      if (token->is_op(OPERATOR_SEMICOLON))
	this->for_clause(&cond, &post);
      else
	{
	  // We might be looking at a Condition, an InitStat, or a
	  // RangeClause.
	  bool saw_send_stmt = false;
	  cond = this->simple_stat(false, &saw_send_stmt, &range_clause, NULL);
	  if (!this->peek_token()->is_op(OPERATOR_SEMICOLON))
	    {
	      if (cond == NULL && !range_clause.found)
		{
		  if (saw_send_stmt)
		    go_error_at(this->location(),
                                ("send statement used as value; "
                                 "use select for non-blocking send"));
		  else
		    go_error_at(this->location(),
                                "parse error in for statement");
		}
	    }
	  else
	    {
	      if (range_clause.found)
		go_error_at(this->location(), "parse error after range clause");

	      if (cond != NULL)
		{
		  // COND is actually an expression statement for
		  // InitStat at the start of a ForClause.
		  this->expression_stat(cond);
		  cond = NULL;
		}

	      this->for_clause(&cond, &post);
	    }
	}
    }

  // Check for the easy error of a newline before starting the block.
  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
    {
      Location semi_loc = this->location();
      if (this->advance_token()->is_op(OPERATOR_LCURLY))
	go_error_at(semi_loc, "unexpected semicolon or newline, expecting %<{%> after for clause");
      // Otherwise we will get an error when we call this->block
      // below.
    }

  // Build the For_statement and note that it is the current target
  // for break and continue statements.

  For_statement* sfor;
  For_range_statement* srange;
  Statement* s;
  if (!range_clause.found)
    {
      sfor = Statement::make_for_statement(init, cond, post, location);
      s = sfor;
      srange = NULL;
    }
  else
    {
      srange = Statement::make_for_range_statement(range_clause.index,
						   range_clause.value,
						   range_clause.range,
						   location);
      s = srange;
      sfor = NULL;
    }

  this->push_break_statement(s, label);
  this->push_continue_statement(s, label);

  // Gather the block of statements in the loop and add them to the
  // For_statement.

  this->gogo_->start_block(this->location());
  Location end_loc = this->block();
  Block* statements = this->gogo_->finish_block(end_loc);

  if (sfor != NULL)
    sfor->add_statements(statements);
  else
    srange->add_statements(statements);

  // This is no longer the break/continue target.
  this->pop_break_statement();
  this->pop_continue_statement();

  // Add the For_statement to the list of statements, and close out
  // the block we started to hold any variables defined in the for
  // statement.

  this->gogo_->add_statement(s);

  this->gogo_->add_block(this->gogo_->finish_block(this->location()),
			 location);
}

// ForClause = [ InitStat ] ";" [ Condition ] ";" [ PostStat ] .
// InitStat = SimpleStat .
// PostStat = SimpleStat .

// We have already read InitStat at this point.

void
Parse::for_clause(Expression** cond, Block** post)
{
  go_assert(this->peek_token()->is_op(OPERATOR_SEMICOLON));
  this->advance_token();
  if (this->peek_token()->is_op(OPERATOR_SEMICOLON))
    *cond = NULL;
  else if (this->peek_token()->is_op(OPERATOR_LCURLY))
    {
      go_error_at(this->location(), "unexpected semicolon or newline, expecting %<{%> after for clause");
      *cond = NULL;
      *post = NULL;
      return;
    }
  else
    *cond = this->expression(PRECEDENCE_NORMAL, false, true, NULL, NULL);
  if (!this->peek_token()->is_op(OPERATOR_SEMICOLON))
    go_error_at(this->location(), "expected semicolon");
  else
    this->advance_token();

  if (this->peek_token()->is_op(OPERATOR_LCURLY))
    *post = NULL;
  else
    {
      this->gogo_->start_block(this->location());
      this->simple_stat(false, NULL, NULL, NULL);
      *post = this->gogo_->finish_block(this->location());
    }
}

// RangeClause = [ IdentifierList ( "=" | ":=" ) ] "range" Expression .

// This is the := version.  It is called with a list of identifiers.

void
Parse::range_clause_decl(const Typed_identifier_list* til,
			 Range_clause* p_range_clause)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_RANGE));
  Location location = this->location();

  p_range_clause->found = true;

  if (til->size() > 2)
    go_error_at(this->location(), "too many variables for range clause");

  this->advance_token();
  Expression* expr = this->expression(PRECEDENCE_NORMAL, false, false, NULL,
				      NULL);
  p_range_clause->range = expr;

  if (til->empty())
    return;

  bool any_new = false;

  const Typed_identifier* pti = &til->front();
  Named_object* no = this->init_var(*pti, NULL, expr, true, true, &any_new,
				    NULL, NULL);
  if (any_new && no->is_variable())
    no->var_value()->set_type_from_range_index();
  p_range_clause->index = Expression::make_var_reference(no, location);

  if (til->size() == 1)
    p_range_clause->value = NULL;
  else
    {
      pti = &til->back();
      bool is_new = false;
      no = this->init_var(*pti, NULL, expr, true, true, &is_new, NULL, NULL);
      if (is_new && no->is_variable())
	no->var_value()->set_type_from_range_value();
      if (is_new)
	any_new = true;
      p_range_clause->value = Expression::make_var_reference(no, location);
    }

  if (!any_new)
    go_error_at(location, "variables redeclared but no variable is new");
}

// The = version of RangeClause.  This is called with a list of
// expressions.

void
Parse::range_clause_expr(const Expression_list* vals,
			 Range_clause* p_range_clause)
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_RANGE));

  p_range_clause->found = true;

  go_assert(vals->size() >= 1);
  if (vals->size() > 2)
    go_error_at(this->location(), "too many variables for range clause");

  this->advance_token();
  p_range_clause->range = this->expression(PRECEDENCE_NORMAL, false, false,
					   NULL, NULL);

  if (vals->empty())
    return;

  p_range_clause->index = vals->front();
  if (vals->size() == 1)
    p_range_clause->value = NULL;
  else
    p_range_clause->value = vals->back();
}

// Push a statement on the break stack.

void
Parse::push_break_statement(Statement* enclosing, Label* label)
{
  if (this->break_stack_ == NULL)
    this->break_stack_ = new Bc_stack();
  this->break_stack_->push_back(std::make_pair(enclosing, label));
}

// Push a statement on the continue stack.

void
Parse::push_continue_statement(Statement* enclosing, Label* label)
{
  if (this->continue_stack_ == NULL)
    this->continue_stack_ = new Bc_stack();
  this->continue_stack_->push_back(std::make_pair(enclosing, label));
}

// Pop the break stack.

void
Parse::pop_break_statement()
{
  this->break_stack_->pop_back();
}

// Pop the continue stack.

void
Parse::pop_continue_statement()
{
  this->continue_stack_->pop_back();
}

// Find a break or continue statement given a label name.

Statement*
Parse::find_bc_statement(const Bc_stack* bc_stack, const std::string& label)
{
  if (bc_stack == NULL)
    return NULL;
  for (Bc_stack::const_reverse_iterator p = bc_stack->rbegin();
       p != bc_stack->rend();
       ++p)
    {
      if (p->second != NULL && p->second->name() == label)
	{
	  p->second->set_is_used();
	  return p->first;
	}
    }
  return NULL;
}

// BreakStat = "break" [ identifier ] .

void
Parse::break_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_BREAK));
  Location location = this->location();

  const Token* token = this->advance_token();
  Statement* enclosing;
  if (!token->is_identifier())
    {
      if (this->break_stack_ == NULL || this->break_stack_->empty())
	{
	  go_error_at(this->location(),
                      "break statement not within for or switch or select");
	  return;
	}
      enclosing = this->break_stack_->back().first;
    }
  else
    {
      enclosing = this->find_bc_statement(this->break_stack_,
					  token->identifier());
      if (enclosing == NULL)
	{
	  // If there is a label with this name, mark it as used to
	  // avoid a useless error about an unused label.
	  this->gogo_->add_label_reference(token->identifier(),
                                           Linemap::unknown_location(), false);

	  go_error_at(token->location(), "invalid break label %qs",
                      Gogo::message_name(token->identifier()).c_str());
	  this->advance_token();
	  return;
	}
      this->advance_token();
    }

  Unnamed_label* label;
  if (enclosing->classification() == Statement::STATEMENT_FOR)
    label = enclosing->for_statement()->break_label();
  else if (enclosing->classification() == Statement::STATEMENT_FOR_RANGE)
    label = enclosing->for_range_statement()->break_label();
  else if (enclosing->classification() == Statement::STATEMENT_SWITCH)
    label = enclosing->switch_statement()->break_label();
  else if (enclosing->classification() == Statement::STATEMENT_TYPE_SWITCH)
    label = enclosing->type_switch_statement()->break_label();
  else if (enclosing->classification() == Statement::STATEMENT_SELECT)
    label = enclosing->select_statement()->break_label();
  else
    go_unreachable();

  this->gogo_->add_statement(Statement::make_break_statement(label,
							     location));
}

// ContinueStat = "continue" [ identifier ] .

void
Parse::continue_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_CONTINUE));
  Location location = this->location();

  const Token* token = this->advance_token();
  Statement* enclosing;
  if (!token->is_identifier())
    {
      if (this->continue_stack_ == NULL || this->continue_stack_->empty())
	{
	  go_error_at(this->location(), "continue statement not within for");
	  return;
	}
      enclosing = this->continue_stack_->back().first;
    }
  else
    {
      enclosing = this->find_bc_statement(this->continue_stack_,
					  token->identifier());
      if (enclosing == NULL)
	{
	  // If there is a label with this name, mark it as used to
	  // avoid a useless error about an unused label.
	  this->gogo_->add_label_reference(token->identifier(),
                                           Linemap::unknown_location(), false);

	  go_error_at(token->location(), "invalid continue label %qs",
                      Gogo::message_name(token->identifier()).c_str());
	  this->advance_token();
	  return;
	}
      this->advance_token();
    }

  Unnamed_label* label;
  if (enclosing->classification() == Statement::STATEMENT_FOR)
    label = enclosing->for_statement()->continue_label();
  else if (enclosing->classification() == Statement::STATEMENT_FOR_RANGE)
    label = enclosing->for_range_statement()->continue_label();
  else
    go_unreachable();

  this->gogo_->add_statement(Statement::make_continue_statement(label,
								location));
}

// GotoStat = "goto" identifier .

void
Parse::goto_stat()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_GOTO));
  Location location = this->location();
  const Token* token = this->advance_token();
  if (!token->is_identifier())
    go_error_at(this->location(), "expected label for goto");
  else
    {
      Label* label = this->gogo_->add_label_reference(token->identifier(),
						      location, true);
      Statement* s = Statement::make_goto_statement(label, location);
      this->gogo_->add_statement(s);
      this->advance_token();
    }
}

// PackageClause = "package" PackageName .

void
Parse::package_clause()
{
  const Token* token = this->peek_token();
  Location location = token->location();
  std::string name;
  if (!token->is_keyword(KEYWORD_PACKAGE))
    {
      go_error_at(this->location(), "program must start with package clause");
      name = "ERROR";
    }
  else
    {
      token = this->advance_token();
      if (token->is_identifier())
	{
	  name = token->identifier();
	  if (name == "_")
	    {
	      go_error_at(this->location(), "invalid package name %<_%>");
	      name = Gogo::erroneous_name();
	    }
	  this->advance_token();
	}
      else
	{
	  go_error_at(this->location(), "package name must be an identifier");
	  name = "ERROR";
	}
    }
  this->gogo_->set_package_name(name, location);
}

// ImportDecl = "import" Decl<ImportSpec> .

void
Parse::import_decl()
{
  go_assert(this->peek_token()->is_keyword(KEYWORD_IMPORT));
  this->advance_token();
  this->decl(&Parse::import_spec);
}

// ImportSpec = [ "." | PackageName ] PackageFileName .

void
Parse::import_spec()
{
  this->check_directives();

  const Token* token = this->peek_token();
  Location location = token->location();

  std::string local_name;
  bool is_local_name_exported = false;
  if (token->is_op(OPERATOR_DOT))
    {
      local_name = ".";
      token = this->advance_token();
    }
  else if (token->is_identifier())
    {
      local_name = token->identifier();
      is_local_name_exported = token->is_identifier_exported();
      token = this->advance_token();
    }

  if (!token->is_string())
    {
      go_error_at(this->location(), "import path must be a string");
      this->advance_token();
      return;
    }

  this->gogo_->import_package(token->string_value(), local_name,
			      is_local_name_exported, true, location);

  this->advance_token();
}

// SourceFile       = PackageClause ";" { ImportDecl ";" }
//			{ TopLevelDecl ";" } .

void
Parse::program()
{
  this->package_clause();

  const Token* token = this->peek_token();
  if (token->is_op(OPERATOR_SEMICOLON))
    token = this->advance_token();
  else
    go_error_at(this->location(),
                "expected %<;%> or newline after package clause");

  while (token->is_keyword(KEYWORD_IMPORT))
    {
      this->import_decl();
      token = this->peek_token();
      if (token->is_op(OPERATOR_SEMICOLON))
	token = this->advance_token();
      else
	go_error_at(this->location(),
                    "expected %<;%> or newline after import declaration");
    }

  while (!token->is_eof())
    {
      if (this->declaration_may_start_here())
	this->declaration();
      else
	{
	  go_error_at(this->location(), "expected declaration");
	  this->gogo_->mark_locals_used();
	  do
	    this->advance_token();
	  while (!this->peek_token()->is_eof()
		 && !this->peek_token()->is_op(OPERATOR_SEMICOLON)
		 && !this->peek_token()->is_op(OPERATOR_RCURLY));
	  if (!this->peek_token()->is_eof()
	      && !this->peek_token()->is_op(OPERATOR_SEMICOLON))
	    this->advance_token();
	}
      token = this->peek_token();
      if (token->is_op(OPERATOR_SEMICOLON))
	token = this->advance_token();
      else if (!token->is_eof() || !saw_errors())
	{
	  if (token->is_op(OPERATOR_CHANOP))
	    go_error_at(this->location(),
                        ("send statement used as value; "
                         "use select for non-blocking send"));
	  else
	    go_error_at(this->location(),
                        ("expected %<;%> or newline after top "
                         "level declaration"));
	  this->skip_past_error(OPERATOR_INVALID);
	}
    }

  this->check_directives();
}

// If there are any pending compiler directives, clear them and give
// an error.  This is called when directives are not permitted.

void
Parse::check_directives()
{
  if (this->lex_->get_and_clear_pragmas() != 0)
    go_error_at(this->location(), "misplaced compiler directive");
  if (this->lex_->has_embeds())
    {
      this->lex_->clear_embeds();
      go_error_at(this->location(), "misplaced go:embed directive");
    }
}

// Skip forward to a semicolon or OP.  OP will normally be
// OPERATOR_RPAREN or OPERATOR_RCURLY.  If we find a semicolon, move
// past it and return.  If we find OP, it will be the next token to
// read.  Return true if we are OK, false if we found EOF.

bool
Parse::skip_past_error(Operator op)
{
  this->gogo_->mark_locals_used();
  const Token* token = this->peek_token();
  while (!token->is_op(op))
    {
      if (token->is_eof())
	return false;
      if (token->is_op(OPERATOR_SEMICOLON))
	{
	  this->advance_token();
	  return true;
	}
      token = this->advance_token();
    }
  return true;
}

// Check that an expression is not a sink.

Expression*
Parse::verify_not_sink(Expression* expr)
{
  if (expr->is_sink_expression())
    {
      go_error_at(expr->location(), "cannot use %<_%> as value");
      expr = Expression::make_error(expr->location());
    }

  // If this can not be a sink, and it is a variable, then we are
  // using the variable, not just assigning to it.
  if (expr->var_expression() != NULL)
    this->mark_var_used(expr->var_expression()->named_object());
  else if (expr->enclosed_var_expression() != NULL)
    this->mark_var_used(expr->enclosed_var_expression()->variable());
  return expr;
}

// Mark a variable as used.

void
Parse::mark_var_used(Named_object* no)
{
  if (no->is_variable())
    no->var_value()->set_is_used();
}
