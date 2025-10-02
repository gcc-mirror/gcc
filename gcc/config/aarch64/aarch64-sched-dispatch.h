/* Dispatch scheduling hooks for AArch64.
   Copyright The GNU Toolchain Authors.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_SCHED_DISPATCH_H
#define GCC_AARCH64_SCHED_DISPATCH_H

void aarch64_sched_dispatch_do (rtx_insn *, int);
bool aarch64_sched_dispatch (rtx_insn *, int);

/* Describes a dispatch window and keeps track of the dispatch constraints.
   The constraints are represented as array of slot counts, where each
   index corresponds to a dispatch constraint type. */
class dispatch_window
{
public:
  dispatch_window (const dispatch_constraint_info &constraint_info);
  ~dispatch_window ();

  bool fits_window (rtx_insn *insn,
		    const vec<std::pair<int, int>> &constraints) const;
  vec<std::pair<int, int>> get_constraints (rtx_insn *) const;
  void add_insn_to_window (rtx_insn *);
  bool has_violation () const;
  void print_window (FILE *) const;

private:
  void reset_constraints ();

  const int *m_max_slots;
  int *m_free_slots;
  const int m_num_constraints;
  vec<std::pair<int, int>> (*const m_callback) (rtx_insn *);
  bool m_violation;
};

#endif /* GCC_AARCH64_SCHED_DISPATCH_H */
