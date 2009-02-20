/* Communication between the Integrated Register Allocator (IRA) and
   the rest of the compiler.
   Copyright (C) 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

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

/* True if we have allocno conflicts.  It is false for non-optimized
   mode or when the conflict table is too big.  */
extern bool ira_conflicts_p;

extern void ira_init_once (void);
extern void ira_init (void);
extern void ira_finish_once (void);
extern rtx ira_eliminate_regs (rtx, enum machine_mode);

extern void ira_sort_regnos_for_alter_reg (int *, int, unsigned int *);
extern void ira_mark_allocation_change (int);
extern void ira_mark_memory_move_deletion (int, int);
extern bool ira_reassign_pseudos (int *, int, HARD_REG_SET, HARD_REG_SET *,
				  HARD_REG_SET *, bitmap);
extern rtx ira_reuse_stack_slot (int, unsigned int, unsigned int);
extern void ira_mark_new_stack_slot (rtx, int, unsigned int);
extern bool ira_better_spill_reload_regno_p (int *, int *, rtx, rtx, rtx);

