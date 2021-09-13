/* Search for references that a functions loads or stores.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.

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

#ifndef IPA_MODREF_H
#define IPA_MODREF_H

typedef modref_tree <alias_set_type> modref_records;
typedef unsigned char eaf_flags_t;

/* Single function summary.  */

struct GTY(()) modref_summary
{
  /* Load and stores in function (transitively closed to all callees)  */
  modref_records *loads;
  modref_records *stores;
  auto_vec<eaf_flags_t> GTY((skip)) arg_flags;
  bool writes_errno;

  modref_summary ();
  ~modref_summary ();
  void dump (FILE *);
  bool useful_p (int ecf_flags, bool check_flags = true);
};

modref_summary *get_modref_function_summary (cgraph_node *func);
void ipa_modref_c_finalize ();
void ipa_merge_modref_summary_after_inlining (cgraph_edge *e);

#endif
