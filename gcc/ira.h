/* Communication between the Integrated Register Allocator (IRA) and
   the rest of the compiler.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
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

/* Function specific hard registers can not be used for the register
   allocation.  */
extern HARD_REG_SET ira_no_alloc_regs;

/* True if we have allocno conflicts.  It is false for non-optimized
   mode or when the conflict table is too big.  */
extern bool ira_conflicts_p;

struct target_ira {
  /* Number of given class hard registers available for the register
     allocation for given classes.  */
  int x_ira_available_class_regs[N_REG_CLASSES];

  /* Map: hard register number -> cover class it belongs to.  If the
     corresponding class is NO_REGS, the hard register is not available
     for allocation.  */
  enum reg_class x_ira_hard_regno_cover_class[FIRST_PSEUDO_REGISTER];

  /* Number of cover classes.  Cover classes is non-intersected register
     classes containing all hard-registers available for the
     allocation.  */
  int x_ira_reg_class_cover_size;

  /* The array containing cover classes (see also comments for macro
     IRA_COVER_CLASSES;.  Only first IRA_REG_CLASS_COVER_SIZE elements are
     used for this.  */
  enum reg_class x_ira_reg_class_cover[N_REG_CLASSES];

  /* Map of all register classes to corresponding cover class containing
     the given class.  If given class is not a subset of a cover class,
     we translate it into the cheapest cover class.  */
  enum reg_class x_ira_class_translate[N_REG_CLASSES];

  /* Map: register class x machine mode -> number of hard registers of
     given class needed to store value of given mode.  If the number for
     some hard-registers of the register class is different, the size
     will be negative.  */
  int x_ira_reg_class_nregs[N_REG_CLASSES][MAX_MACHINE_MODE];

  /* Array analogous to target hook TARGET_MEMORY_MOVE_COST.  */
  short x_ira_memory_move_cost[MAX_MACHINE_MODE][N_REG_CLASSES][2];

  /* Array of number of hard registers of given class which are
     available for the allocation.  The order is defined by the
     allocation order.  */
  short x_ira_class_hard_regs[N_REG_CLASSES][FIRST_PSEUDO_REGISTER];

  /* The number of elements of the above array for given register
     class.  */
  int x_ira_class_hard_regs_num[N_REG_CLASSES];
};

extern struct target_ira default_target_ira;
#if SWITCHABLE_TARGET
extern struct target_ira *this_target_ira;
#else
#define this_target_ira (&default_target_ira)
#endif

#define ira_available_class_regs \
  (this_target_ira->x_ira_available_class_regs)
#define ira_hard_regno_cover_class \
  (this_target_ira->x_ira_hard_regno_cover_class)
#define ira_reg_class_cover_size \
  (this_target_ira->x_ira_reg_class_cover_size)
#define ira_reg_class_cover \
  (this_target_ira->x_ira_reg_class_cover)
#define ira_class_translate \
  (this_target_ira->x_ira_class_translate)
#define ira_reg_class_nregs \
  (this_target_ira->x_ira_reg_class_nregs)
#define ira_memory_move_cost \
  (this_target_ira->x_ira_memory_move_cost)
#define ira_class_hard_regs \
  (this_target_ira->x_ira_class_hard_regs)
#define ira_class_hard_regs_num \
  (this_target_ira->x_ira_class_hard_regs_num)

extern void ira_init_once (void);
extern void ira_init (void);
extern void ira_finish_once (void);
extern void ira_setup_eliminable_regset (void);
extern rtx ira_eliminate_regs (rtx, enum machine_mode);
extern void ira_set_pseudo_classes (FILE *);
extern void ira_implicitly_set_insn_hard_regs (HARD_REG_SET *);

extern void ira_sort_regnos_for_alter_reg (int *, int, unsigned int *);
extern void ira_mark_allocation_change (int);
extern void ira_mark_memory_move_deletion (int, int);
extern bool ira_reassign_pseudos (int *, int, HARD_REG_SET, HARD_REG_SET *,
				  HARD_REG_SET *, bitmap);
extern rtx ira_reuse_stack_slot (int, unsigned int, unsigned int);
extern void ira_mark_new_stack_slot (rtx, int, unsigned int);
extern bool ira_better_spill_reload_regno_p (int *, int *, rtx, rtx, rtx);
extern bool ira_bad_reload_regno (int, rtx, rtx);

extern void ira_adjust_equiv_reg_cost (unsigned, int);
