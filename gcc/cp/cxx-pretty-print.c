/* Implementation of subroutines for the GNU C++ pretty-printer.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "real.h"
#include "cxx-pretty-print.h"
#include "cp-tree.h"

/* Declarations.  */

void
pp_cxx_declaration (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_declaration_specifiers (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_type_specifier (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_declarator (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_direct_declarator (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_parameter_declaration (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_type_id (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

/* Statements.  */

void
pp_cxx_statement (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

/* Expressions. */

static void
pp_cxx_primary_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_postfix_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_unary_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_initializer (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_multiplicatice_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_conditional_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

static void
pp_cxx_assignment_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}

void
pp_cxx_expression (cxx_pretty_printer *pp, tree t)
{
  pp_unsupported_tree (pp, t);
}



typedef c_pretty_print_fn pp_fun;

void
pp_cxx_pretty_printer_init (cxx_pretty_printer *pp)
{
  pp_c_pretty_printer_init (pp_c_base (pp));

  pp->c_base.declaration = (pp_fun) pp_cxx_declaration;
  pp->c_base.declaration_specifiers = (pp_fun) pp_cxx_declaration_specifiers;
  pp->c_base.type_specifier = (pp_fun) pp_cxx_type_specifier;
  pp->c_base.declarator = (pp_fun) pp_cxx_declarator;
  pp->c_base.direct_declarator = (pp_fun) pp_cxx_direct_declarator;
  pp->c_base.parameter_declaration = (pp_fun) pp_cxx_parameter_declaration;
  pp->c_base.type_id = (pp_fun) pp_cxx_type_id;
  pp->c_base.statement = (pp_fun) pp_cxx_statement;
  pp->c_base.primary_expression = (pp_fun) pp_cxx_primary_expression;
  pp->c_base.postfix_expression = (pp_fun) pp_cxx_postfix_expression;
  pp->c_base.unary_expression = (pp_fun) pp_cxx_unary_expression;
  pp->c_base.initializer = (pp_fun) pp_cxx_initializer;
  pp->c_base.multiplicative_expression = (pp_fun) pp_cxx_multiplicatice_expression;
  pp->c_base.conditional_expression = (pp_fun) pp_cxx_conditional_expression;
  pp->c_base.assignment_expression = (pp_fun) pp_cxx_assignment_expression;
  pp->enclosing_scope = NULL;
}
