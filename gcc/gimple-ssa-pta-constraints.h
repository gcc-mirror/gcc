/* Constraint builder for tree based points-to analysis
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GIMPLE_SSA_PTA_CONSTRAINTS_H
#define GIMPLE_SSA_PTA_CONSTRAINTS_H

namespace pointer_analysis {

varinfo_t lookup_vi_for_tree (tree);
varinfo_t lookup_call_use_vi (gcall *);
varinfo_t lookup_call_clobber_vi (gcall *);
varinfo_t get_fi_for_callee (gcall *);

void init_constraint_builder (void);
void delete_constraint_builder (void);
void intra_build_constraints (void);
void ipa_build_constraints (void);

} // namespace pointer_analysis

#endif /* GIMPLE_SSA_PTA_CONSTRAINTS_H  */
