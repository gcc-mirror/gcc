// ast-dump.cc -- AST debug dump.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <iostream>
#include <fstream>

#include "gogo.h"
#include "expressions.h"
#include "statements.h"
#include "types.h"
#include "ast-dump.h"
#include "go-c.h"
#include "go-dump.h"

// The -fgo-dump-ast flag to activate AST dumps.

Go_dump ast_dump_flag("ast");

// This class is used to traverse the tree to look for blocks and
// function headers.

class Ast_dump_traverse_blocks_and_functions : public Traverse
{
 public:
  Ast_dump_traverse_blocks_and_functions(Ast_dump_context* ast_dump_context)
      : Traverse(traverse_blocks | traverse_functions),
      ast_dump_context_(ast_dump_context)
  { }

 protected:
  int
  block(Block*);

  int
  function(Named_object*);

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
          Named_object* no = (*it);

          this->ast_dump_context_->ostream() << no->name() << " ";
          go_assert(no->is_result_variable());
          Result_variable* resvar = no->result_var_value();

          this->ast_dump_context_->dump_type(resvar->type());

        }
      this->ast_dump_context_->ostream() << ")";
    }

  this->ast_dump_context_->ostream() << " : ";
  this->ast_dump_context_->dump_type(func->type());
  this->ast_dump_context_->ostream() << std::endl;

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
      error("cannot open %s:%m, -fgo-dump-ast ignored", dumpname.c_str());
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
      this->ostream() << "(" << t->mangled_name(this->gogo_) <<  ")";
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

// Dump statment to stream.

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
