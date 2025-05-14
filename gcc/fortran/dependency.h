/* Header for dependency analysis
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/****************************** Enums *********************************/
enum gfc_dep_check
{
  NOT_ELEMENTAL,        /* Not elemental case: normal dependency check.  */
  ELEM_CHECK_VARIABLE,  /* Test whether variables overlap.  */
  ELEM_DONT_CHECK_VARIABLE  /* Test whether variables overlap only if used
			       in an expression.  */
};

/*********************** Functions prototypes **************************/

bool gfc_ref_needs_temporary_p (gfc_ref *);
bool gfc_full_array_ref_p (gfc_ref *, bool *);
gfc_expr *gfc_get_noncopying_intrinsic_argument (gfc_expr *);
bool gfc_check_fncall_dependency (gfc_expr *, sym_intent, gfc_symbol *,
				 gfc_actual_arglist *, gfc_dep_check);
int gfc_check_dependency (gfc_expr *, gfc_expr *, bool);
int gfc_expr_is_one (gfc_expr *, int);

bool gfc_dep_resolver (gfc_ref *, gfc_ref *, gfc_reverse *,
		      bool identical = false);
bool gfc_are_equivalenced_arrays (gfc_expr *, gfc_expr *);
bool gfc_omp_expr_prefix_same (gfc_expr *, gfc_expr *);
bool gfc_contains_implied_index_p (gfc_expr *);

gfc_expr * gfc_discard_nops (gfc_expr *);

bool gfc_function_dependency (gfc_symbol *, gfc_symbol *);
