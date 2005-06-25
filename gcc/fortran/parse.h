/* Parser header
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */


#ifndef GFC_PARSE_H
#define GFC_PARSE_H

#include "gfortran.h"

/* Enum for what the compiler is currently doing.  */
typedef enum
{
  COMP_NONE, COMP_PROGRAM, COMP_MODULE, COMP_SUBROUTINE, COMP_FUNCTION,
  COMP_BLOCK_DATA, COMP_INTERFACE, COMP_DERIVED, COMP_IF, COMP_DO,
  COMP_SELECT, COMP_FORALL, COMP_WHERE, COMP_CONTAINS
}
gfc_compile_state;

/* Stack element for the current compilation state.  These structures
   are allocated as automatic variables.  */
typedef struct gfc_state_data
{
  gfc_compile_state state;
  gfc_symbol *sym;              /* Block name associated with this level */
  gfc_symtree *do_variable;     /* For DO blocks the iterator variable.  */

  struct gfc_code *head, *tail;
  struct gfc_state_data *previous;

  /* Block-specific state data.  */
  union
  {
    gfc_st_label *end_do_label;
  }
  ext;
}
gfc_state_data;

extern gfc_state_data *gfc_state_stack;

#define gfc_current_block() (gfc_state_stack->sym)
#define gfc_current_state() (gfc_state_stack->state)

int gfc_check_do_variable (gfc_symtree *);
try gfc_find_state (gfc_compile_state);
gfc_state_data *gfc_enclosing_unit (gfc_compile_state *);
const char *gfc_ascii_statement (gfc_statement);
const char *gfc_state_name (gfc_compile_state);

#endif  /* GFC_PARSE_H  */
