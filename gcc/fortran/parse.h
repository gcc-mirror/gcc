/* Parser header
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#ifndef GFC_PARSE_H
#define GFC_PARSE_H

/* Enum for what the compiler is currently doing.  */
enum gfc_compile_state
{
  COMP_NONE, COMP_PROGRAM, COMP_MODULE, COMP_SUBMODULE, COMP_SUBROUTINE,
  COMP_FUNCTION, COMP_BLOCK_DATA, COMP_INTERFACE, COMP_DERIVED,
  COMP_DERIVED_CONTAINS, COMP_BLOCK, COMP_ASSOCIATE, COMP_IF,
  COMP_STRUCTURE, COMP_UNION, COMP_MAP,
  COMP_DO, COMP_SELECT, COMP_FORALL, COMP_WHERE, COMP_CONTAINS, COMP_ENUM,
  COMP_SELECT_TYPE, COMP_SELECT_RANK, COMP_OMP_STRUCTURED_BLOCK, COMP_CRITICAL,
  COMP_DO_CONCURRENT, COMP_OMP_STRICTLY_STRUCTURED_BLOCK
};

/* Stack element for the current compilation state.  These structures
   are allocated as automatic variables.  */
typedef struct gfc_state_data
{
  gfc_compile_state state;
  gfc_symbol *sym;              /* Block name associated with this level */
  gfc_symtree *do_variable;     /* For DO blocks the iterator variable.  */

  struct gfc_code *construct;
  struct gfc_code *head, *tail;
  struct gfc_state_data *previous;

  /* Block-specific state data.  */
  union
  {
    gfc_st_label *end_do_label;
    gfc_oacc_declare *oacc_declare_clauses;
  }
  ext;
}
gfc_state_data;

extern gfc_state_data *gfc_state_stack;

#define gfc_current_block() (gfc_state_stack->sym)
#define gfc_current_state() (gfc_state_stack->state)
#define gfc_comp_struct(s) \
  ((s) == COMP_DERIVED || (s) == COMP_STRUCTURE || (s) == COMP_MAP)

bool gfc_check_do_variable (gfc_symtree *);
bool gfc_find_state (gfc_compile_state);
gfc_state_data *gfc_enclosing_unit (gfc_compile_state *);
const char *gfc_ascii_statement (gfc_statement, bool strip_sentinel = false) ;
match gfc_match_enum (void);
match gfc_match_enumerator_def (void);
void gfc_free_enum_history (void);
extern bool gfc_matching_function;
match gfc_match_prefix (gfc_typespec *);
bool is_oacc (gfc_state_data *);
#endif  /* GFC_PARSE_H  */
