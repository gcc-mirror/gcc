/* Various declarations for the C and C++ pretty-printers.
   Copyright (C) 2002 Free Software Foundation, Inc.
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

#ifndef GCC_C_PRETTY_PRINTER
#define GCC_C_PRETTY_PRINTER

#include "tree.h"
#include "c-common.h"
#include "pretty-print.h"


/* The data type used to bundle information necessary for pretty-printing
   a C or C++ entity.  */
typedef struct c_pretty_print_info *c_pretty_printer;

/* The type of a C pretty-printer 'member' function.  */
typedef void (*c_pretty_print_fn) PARAMS ((c_pretty_printer, tree));

struct c_pretty_print_info
{
  struct pretty_print_info base;
  /* Points to the first element of an array of offset-list.
     Not used yet.  */
  int *offset_list;

  /* These must be overriden by each of the C and C++ front-end to
     reflect their understanding of syntatic productions when they differ.  */
  c_pretty_print_fn declaration;
  c_pretty_print_fn declaration_specifiers;
  c_pretty_print_fn type_specifier;
  c_pretty_print_fn declarator;
  c_pretty_print_fn direct_declarator;
  c_pretty_print_fn parameter_declaration;
  c_pretty_print_fn type_id;

  c_pretty_print_fn statement;

  c_pretty_print_fn primary_expression;
  c_pretty_print_fn postfix_expression;
  c_pretty_print_fn unary_expression;
  c_pretty_print_fn initializer;
  c_pretty_print_fn multiplicative_expression;
  c_pretty_print_fn conditional_expression;
  c_pretty_print_fn assignment_expression;
};

#define pp_c_left_paren(PPI)                       \
   do {                                            \
     pp_left_paren (PPI);                          \
     pp_c_base (PPI)->base.padding = pp_none;      \
   } while (0)
#define pp_c_right_paren(PPI)                      \
   do {                                            \
     pp_right_paren (PPI);                         \
     pp_c_base (PPI)->base.padding = pp_none;      \
   } while (0)
#define pp_c_left_bracket(PPI)                     \
   do {                                            \
     pp_left_bracket (PPI);                        \
     pp_c_base (PPI)->base.padding = pp_none;      \
   } while (0)
#define pp_c_right_bracket(PPI)                    \
   do {                                            \
     pp_right_bracket (PPI);                       \
     pp_c_base (PPI)->base.padding = pp_none;      \
   } while (0)
#define pp_c_whitespace(PPI)                       \
   do {                                            \
     pp_whitespace (PPI);                          \
     pp_c_base (PPI)->base.padding = pp_none;      \
   } while (0)
#define pp_c_maybe_whitespace(PPI)                 \
   do {                                            \
     if (pp_c_base (PPI)->base.padding != pp_none) \
       pp_c_whitespace (PPI);                      \
   } while (0)
#define pp_c_identifier(PPI, ID)                   \
   do {                                            \
     pp_c_maybe_whitespace (PPI);                  \
     pp_identifier (PPI, ID);                      \
     pp_c_base (PPI)->base.padding = pp_before;    \
   } while (0)
     
#define pp_c_tree_identifier(PPI, ID)              \
   pp_c_identifier (PPI, IDENTIFIER_POINTER (ID))

/* Returns the 'output_buffer *' associated with a PRETTY-PRINTER, the latter
   being something digestible by pp_c_base.  */
#define pp_buffer(PPI) pp_c_base (PPI)->base.buffer

#define pp_declaration(PPI, T)                    \
   (*pp_c_base (PPI)->declaration) (pp_c_base (PPI), T)
#define pp_declaration_specifiers(PPI, D)         \
   (*pp_c_base (PPI)->declaration_specifiers) (pp_c_base (PPI), D)
#define pp_type_specifier(PPI, D)                 \
   (*pp_c_base (PPI)->type_specifier) (pp_c_base (PPI), D)
#define pp_declarator(PPI, D)                     \
   (*pp_c_base (PPI)->declarator) (pp_c_base (PPI), D)
#define pp_direct_declarator(PPI, D)              \
   (*pp_c_base (PPI)->direct_declarator) (pp_c_base (PPI), D)
#define pp_parameter_declaration(PPI, T)          \
  (*pp_c_base (PPI)->parameter_declaration) (pp_c_base (PPI), T)
#define pp_type_id(PPI, D)                        \
  (*pp_c_base (PPI)->type_id) (pp_c_base (PPI), D)

#define pp_statement(PPI, S)                      \
  (*pp_c_base (PPI)->statement) (pp_c_base (PPI), S)

#define pp_primary_expression(PPI, E)             \
  (*pp_c_base (PPI)->primary_expression) (pp_c_base (PPI), E)
#define pp_postfix_expression(PPI, E)             \
  (*pp_c_base (PPI)->postfix_expression) (pp_c_base (PPI), E)
#define pp_unary_expression(PPI, E)               \
  (*pp_c_base (PPI)->unary_expression) (pp_c_base (PPI), E)
#define pp_initializer(PPI, E)                    \
  (*pp_c_base (PPI)->initializer) (pp_c_base (PPI), E)
#define pp_multiplicative_expression(PPI, E)      \
   (*pp_c_base (PPI)->multiplicative_expression) (pp_c_base (PPI), E)
#define pp_conditional_expression(PPI, E)         \
   (*pp_c_base (PPI)->conditional_expression) (pp_c_base (PPI), E)
#define pp_assignment_expression(PPI, E)          \
   (*pp_c_base (PPI)->assignment_expression) (pp_c_base (PPI), E)


/* Returns the c_pretty_printer base object of PRETTY-PRINTER.  This
   macro must be overriden by any subclass of c_pretty_print_info.  */
#define pp_c_base(PP)  (PP)

extern void pp_c_pretty_printer_init   PARAMS ((c_pretty_printer));

/* Declarations.  */
void pp_c_attributes                   PARAMS ((c_pretty_printer, tree));
void pp_c_cv_qualifier                 PARAMS ((c_pretty_printer, int));
void pp_c_parameter_declaration_clause PARAMS ((c_pretty_printer, tree));
void pp_c_declaration                  PARAMS ((c_pretty_printer, tree));
/* Statements.  */
void pp_c_statement                    PARAMS ((c_pretty_printer, tree));
/* Expressions.  */
void pp_c_expression                   PARAMS ((c_pretty_printer, tree));
void pp_c_logical_or_expression        PARAMS ((c_pretty_printer, tree));
void pp_c_expression_list              PARAMS ((c_pretty_printer, tree));
void pp_c_cast_expression              PARAMS ((c_pretty_printer, tree));
void pp_c_postfix_expression           PARAMS ((c_pretty_printer, tree));
void pp_c_initializer                  PARAMS ((c_pretty_printer, tree));
void pp_c_literal                      PARAMS ((c_pretty_printer, tree));

#endif /* GCC_C_PRETTY_PRINTER */
