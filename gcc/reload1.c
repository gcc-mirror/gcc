/* Reload pseudo regs into hard regs for insns that require hard regs.
   Copyright (C) 1987, 1988, 1989, 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "obstack.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "flags.h"
#include "expr.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "reload.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"

/* This file contains the reload pass of the compiler, which is
   run after register allocation has been done.  It checks that
   each insn is valid (operands required to be in registers really
   are in registers of the proper class) and fixes up invalid ones
   by copying values temporarily into registers for the insns
   that need them.

   The results of register allocation are described by the vector
   reg_renumber; the insns still contain pseudo regs, but reg_renumber
   can be used to find which hard reg, if any, a pseudo reg is in.

   The technique we always use is to free up a few hard regs that are
   called ``reload regs'', and for each place where a pseudo reg
   must be in a hard reg, copy it temporarily into one of the reload regs.

   All the pseudos that were formerly allocated to the hard regs that
   are now in use as reload regs must be ``spilled''.  This means
   that they go to other hard regs, or to stack slots if no other
   available hard regs can be found.  Spilling can invalidate more
   insns, requiring additional need for reloads, so we must keep checking
   until the process stabilizes.

   For machines with different classes of registers, we must keep track
   of the register class needed for each reload, and make sure that
   we allocate enough reload registers of each class.

   The file reload.c contains the code that checks one insn for
   validity and reports the reloads that it needs.  This file
   is in charge of scanning the entire rtl code, accumulating the
   reload needs, spilling, assigning reload registers to use for
   fixing up each insn, and generating the new insns to copy values
   into the reload registers.  */

/* During reload_as_needed, element N contains a REG rtx for the hard reg
   into which pseudo reg N has been reloaded (perhaps for a previous insn). */
static rtx *reg_last_reload_reg;

/* Elt N nonzero if reg_last_reload_reg[N] has been set in this insn
   for an output reload that stores into reg N.  */
static char *reg_has_output_reload;

/* Indicates which hard regs are reload-registers for an output reload
   in the current insn.  */
static HARD_REG_SET reg_is_output_reload;

/* Element N is the constant value to which pseudo reg N is equivalent,
   or zero if pseudo reg N is not equivalent to a constant.
   find_reloads looks at this in order to replace pseudo reg N
   with the constant it stands for.  */
rtx *reg_equiv_constant;

/* Element N is a memory location to which pseudo reg N is equivalent,
   prior to any register elimination (such as frame pointer to stack
   pointer).  Depending on whether or not it is a valid address, this value
   is transferred to either reg_equiv_address or reg_equiv_mem.  */
rtx *reg_equiv_memory_loc;

/* Element N is the address of stack slot to which pseudo reg N is equivalent.
   This is used when the address is not valid as a memory address
   (because its displacement is too big for the machine.)  */
rtx *reg_equiv_address;

/* Element N is the memory slot to which pseudo reg N is equivalent,
   or zero if pseudo reg N is not equivalent to a memory slot.  */
rtx *reg_equiv_mem;

/* Widest width in which each pseudo reg is referred to (via subreg).  */
static int *reg_max_ref_width;

/* Element N is the insn that initialized reg N from its equivalent
   constant or memory slot.  */
static rtx *reg_equiv_init;

/* During reload_as_needed, element N contains the last pseudo regno
   reloaded into the Nth reload register.  This vector is in parallel
   with spill_regs.  If that pseudo reg occupied more than one register,
   reg_reloaded_contents points to that pseudo for each spill register in
   use; all of these must remain set for an inheritance to occur.  */
static int reg_reloaded_contents[FIRST_PSEUDO_REGISTER];

/* During reload_as_needed, element N contains the insn for which
   the Nth reload register was last used.  This vector is in parallel
   with spill_regs, and its contents are significant only when
   reg_reloaded_contents is significant.  */
static rtx reg_reloaded_insn[FIRST_PSEUDO_REGISTER];

/* Number of spill-regs so far; number of valid elements of spill_regs.  */
static int n_spills;

/* In parallel with spill_regs, contains REG rtx's for those regs.
   Holds the last rtx used for any given reg, or 0 if it has never
   been used for spilling yet.  This rtx is reused, provided it has
   the proper mode.  */
static rtx spill_reg_rtx[FIRST_PSEUDO_REGISTER];

/* In parallel with spill_regs, contains nonzero for a spill reg
   that was stored after the last time it was used.
   The precise value is the insn generated to do the store.  */
static rtx spill_reg_store[FIRST_PSEUDO_REGISTER];

/* This table is the inverse mapping of spill_regs:
   indexed by hard reg number,
   it contains the position of that reg in spill_regs,
   or -1 for something that is not in spill_regs.  */
static short spill_reg_order[FIRST_PSEUDO_REGISTER];

/* This reg set indicates registers that may not be used for retrying global
   allocation.  The registers that may not be used include all spill registers
   and the frame pointer (if we are using one).  */
HARD_REG_SET forbidden_regs;

/* This reg set indicates registers that are not good for spill registers.
   They will not be used to complete groups of spill registers.  This includes
   all fixed registers, registers that may be eliminated, and registers
   explicitly used in the rtl.

   (spill_reg_order prevents these registers from being used to start a
   group.)  */
static HARD_REG_SET bad_spill_regs;

/* Describes order of use of registers for reloading
   of spilled pseudo-registers.  `spills' is the number of
   elements that are actually valid; new ones are added at the end.  */
static short spill_regs[FIRST_PSEUDO_REGISTER];

/* Describes order of preference for putting regs into spill_regs.
   Contains the numbers of all the hard regs, in order most preferred first.
   This order is different for each function.
   It is set up by order_regs_for_reload.
   Empty elements at the end contain -1.  */
static short potential_reload_regs[FIRST_PSEUDO_REGISTER];

/* 1 for a hard register that appears explicitly in the rtl
   (for example, function value registers, special registers
   used by insns, structure value pointer registers).  */
static char regs_explicitly_used[FIRST_PSEUDO_REGISTER];

/* Indicates if a register was counted against the need for
   groups.  0 means it can count against max_nongroup instead.  */
static HARD_REG_SET counted_for_groups;

/* Indicates if a register was counted against the need for
   non-groups.  0 means it can become part of a new group.
   During choose_reload_regs, 1 here means don't use this reg
   as part of a group, even if it seems to be otherwise ok.  */
static HARD_REG_SET counted_for_nongroups;

/* Nonzero if indirect addressing is supported on the machine; this means
   that spilling (REG n) does not require reloading it into a register in
   order to do (MEM (REG n)) or (MEM (PLUS (REG n) (CONST_INT c))).  The
   value indicates the level of indirect addressing supported, e.g., two
   means that (MEM (MEM (REG n))) is also valid if (REG n) does not get
   a hard register.  */

static char spill_indirect_levels;

/* Nonzero if indirect addressing is supported when the innermost MEM is
   of the form (MEM (SYMBOL_REF sym)).  It is assumed that the level to
   which these are valid is the same as spill_indirect_levels, above.   */

char indirect_symref_ok;

/* Nonzero if an address (plus (reg frame_pointer) (reg ...)) is valid.  */

char double_reg_address_ok;

/* Record the stack slot for each spilled hard register.  */

static rtx spill_stack_slot[FIRST_PSEUDO_REGISTER];

/* Width allocated so far for that stack slot.  */

static int spill_stack_slot_width[FIRST_PSEUDO_REGISTER];

/* Indexed by register class and basic block number, nonzero if there is
   any need for a spill register of that class in that basic block.
   The pointer is 0 if we did stupid allocation and don't know
   the structure of basic blocks.  */

char *basic_block_needs[N_REG_CLASSES];

/* First uid used by insns created by reload in this function.
   Used in find_equiv_reg.  */
int reload_first_uid;

/* Flag set by local-alloc or global-alloc if anything is live in
   a call-clobbered reg across calls.  */

int caller_save_needed;

/* Set to 1 while reload_as_needed is operating.
   Required by some machines to handle any generated moves differently.  */

int reload_in_progress = 0;

/* These arrays record the insn_code of insns that may be needed to
   perform input and output reloads of special objects.  They provide a
   place to pass a scratch register.  */

enum insn_code reload_in_optab[NUM_MACHINE_MODES];
enum insn_code reload_out_optab[NUM_MACHINE_MODES];

/* This obstack is used for allocation of rtl during register elimination.
   The allocated storage can be freed once find_reloads has processed the
   insn.  */

struct obstack reload_obstack;
char *reload_firstobj;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* List of labels that must never be deleted.  */
extern rtx forced_labels;

/* This structure is used to record information about register eliminations.
   Each array entry describes one possible way of eliminating a register
   in favor of another.   If there is more than one way of eliminating a
   particular register, the most preferred should be specified first.  */

static struct elim_table
{
  int from;			/* Register number to be eliminated. */
  int to;			/* Register number used as replacement. */
  int initial_offset;		/* Initial difference between values. */
  int can_eliminate;		/* Non-zero if this elimination can be done. */
  int can_eliminate_previous;	/* Value of CAN_ELIMINATE in previous scan over
				   insns made by reload. */
  int offset;			/* Current offset between the two regs. */
  int max_offset;		/* Maximum offset between the two regs. */
  int previous_offset;		/* Offset at end of previous insn. */
  int ref_outside_mem;		/* "to" has been referenced outside a MEM. */
  rtx from_rtx;			/* REG rtx for the register to be eliminated.
				   We cannot simply compare the number since
				   we might then spuriously replace a hard
				   register corresponding to a pseudo
				   assigned to the reg to be eliminated. */
  rtx to_rtx;			/* REG rtx for the replacement. */
} reg_eliminate[] =

/* If a set of eliminable registers was specified, define the table from it.
   Otherwise, default to the normal case of the frame pointer being
   replaced by the stack pointer.  */

#ifdef ELIMINABLE_REGS
  ELIMINABLE_REGS;
#else
  {{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}};
#endif

#define NUM_ELIMINABLE_REGS (sizeof reg_eliminate / sizeof reg_eliminate[0])

/* Record the number of pending eliminations that have an offset not equal
   to their initial offset.  If non-zero, we use a new copy of each
   replacement result in any insns encountered.  */
static int num_not_at_initial_offset;

/* Count the number of registers that we may be able to eliminate.  */
static int num_eliminable;

/* For each label, we record the offset of each elimination.  If we reach
   a label by more than one path and an offset differs, we cannot do the
   elimination.  This information is indexed by the number of the label.
   The first table is an array of flags that records whether we have yet
   encountered a label and the second table is an array of arrays, one
   entry in the latter array for each elimination.  */

static char *offsets_known_at;
static int (*offsets_at)[NUM_ELIMINABLE_REGS];

/* Number of labels in the current function.  */

static int num_labels;

void mark_home_live ();
static void count_possible_groups ();
static int possible_group_p ();
static void scan_paradoxical_subregs ();
static void reload_as_needed ();
static int modes_equiv_for_class_p ();
static void alter_reg ();
static void delete_dead_insn ();
static void spill_failure ();
static int new_spill_reg();
static void set_label_offsets ();
static int eliminate_regs_in_insn ();
static void mark_not_eliminable ();
static int spill_hard_reg ();
static void choose_reload_regs ();
static void emit_reload_insns ();
static void delete_output_reload ();
static void forget_old_reloads_1 ();
static void order_regs_for_reload ();
static rtx inc_for_reload ();
static int constraint_accepts_reg_p ();
static int count_occurrences ();

extern void remove_death ();
extern rtx adj_offsettable_operand ();
extern rtx form_sum ();

void
init_reload ()
{
  register int i;

  /* Often (MEM (REG n)) is still valid even if (REG n) is put on the stack.
     Set spill_indirect_levels to the number of levels such addressing is
     permitted, zero if it is not permitted at all.  */

  register rtx tem
    = gen_rtx (MEM, Pmode,
	       gen_rtx (PLUS, Pmode,
			gen_rtx (REG, Pmode, LAST_VIRTUAL_REGISTER + 1),
			GEN_INT (4)));
  spill_indirect_levels = 0;

  while (memory_address_p (QImode, tem))
    {
      spill_indirect_levels++;
      tem = gen_rtx (MEM, Pmode, tem);
    }

  /* See if indirect addressing is valid for (MEM (SYMBOL_REF ...)).  */

  tem = gen_rtx (MEM, Pmode, gen_rtx (SYMBOL_REF, Pmode, "foo"));
  indirect_symref_ok = memory_address_p (QImode, tem);

  /* See if reg+reg is a valid (and offsettable) address.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      tem = gen_rtx (PLUS, Pmode,
		     gen_rtx (REG, Pmode, FRAME_POINTER_REGNUM),
		     gen_rtx (REG, Pmode, i));
      /* This way, we make sure that reg+reg is an offsettable address.  */
      tem = plus_constant (tem, 4);

      if (memory_address_p (QImode, tem))
	{
	  double_reg_address_ok = 1;
	  break;
	}
    }

  /* Initialize obstack for our rtl allocation. */
  gcc_obstack_init (&reload_obstack);
  reload_firstobj = (char *) obstack_alloc (&reload_obstack, 0);

#ifdef HAVE_SECONDARY_RELOADS

  /* Initialize the optabs for doing special input and output reloads.  */

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    reload_in_optab[i] = reload_out_optab[i] = CODE_FOR_nothing;

#ifdef HAVE_reload_inqi
  if (HAVE_reload_inqi)
    reload_in_optab[(int) QImode] = CODE_FOR_reload_inqi;
#endif
#ifdef HAVE_reload_inhi
  if (HAVE_reload_inhi)
    reload_in_optab[(int) HImode] = CODE_FOR_reload_inhi;
#endif
#ifdef HAVE_reload_insi
  if (HAVE_reload_insi)
    reload_in_optab[(int) SImode] = CODE_FOR_reload_insi;
#endif
#ifdef HAVE_reload_indi
  if (HAVE_reload_indi)
    reload_in_optab[(int) DImode] = CODE_FOR_reload_indi;
#endif
#ifdef HAVE_reload_inti
  if (HAVE_reload_inti)
    reload_in_optab[(int) TImode] = CODE_FOR_reload_inti;
#endif
#ifdef HAVE_reload_insf
  if (HAVE_reload_insf)
    reload_in_optab[(int) SFmode] = CODE_FOR_reload_insf;
#endif
#ifdef HAVE_reload_indf
  if (HAVE_reload_indf)
    reload_in_optab[(int) DFmode] = CODE_FOR_reload_indf;
#endif
#ifdef HAVE_reload_inxf
  if (HAVE_reload_inxf)
    reload_in_optab[(int) XFmode] = CODE_FOR_reload_inxf;
#endif
#ifdef HAVE_reload_intf
  if (HAVE_reload_intf)
    reload_in_optab[(int) TFmode] = CODE_FOR_reload_intf;
#endif

#ifdef HAVE_reload_outqi
  if (HAVE_reload_outqi)
    reload_out_optab[(int) QImode] = CODE_FOR_reload_outqi;
#endif
#ifdef HAVE_reload_outhi
  if (HAVE_reload_outhi)
    reload_out_optab[(int) HImode] = CODE_FOR_reload_outhi;
#endif
#ifdef HAVE_reload_outsi
  if (HAVE_reload_outsi)
    reload_out_optab[(int) SImode] = CODE_FOR_reload_outsi;
#endif
#ifdef HAVE_reload_outdi
  if (HAVE_reload_outdi)
    reload_out_optab[(int) DImode] = CODE_FOR_reload_outdi;
#endif
#ifdef HAVE_reload_outti
  if (HAVE_reload_outti)
    reload_out_optab[(int) TImode] = CODE_FOR_reload_outti;
#endif
#ifdef HAVE_reload_outsf
  if (HAVE_reload_outsf)
    reload_out_optab[(int) SFmode] = CODE_FOR_reload_outsf;
#endif
#ifdef HAVE_reload_outdf
  if (HAVE_reload_outdf)
    reload_out_optab[(int) DFmode] = CODE_FOR_reload_outdf;
#endif
#ifdef HAVE_reload_outxf
  if (HAVE_reload_outxf)
    reload_out_optab[(int) XFmode] = CODE_FOR_reload_outxf;
#endif
#ifdef HAVE_reload_outtf
  if (HAVE_reload_outtf)
    reload_out_optab[(int) TFmode] = CODE_FOR_reload_outtf;
#endif

#endif /* HAVE_SECONDARY_RELOADS */

}

/* Main entry point for the reload pass, and only entry point
   in this file.

   FIRST is the first insn of the function being compiled.

   GLOBAL nonzero means we were called from global_alloc
   and should attempt to reallocate any pseudoregs that we
   displace from hard regs we will use for reloads.
   If GLOBAL is zero, we do not have enough information to do that,
   so any pseudo reg that is spilled must go to the stack.

   DUMPFILE is the global-reg debugging dump file stream, or 0.
   If it is nonzero, messages are written to it to describe
   which registers are seized as reload regs, which pseudo regs
   are spilled from them, and where the pseudo regs are reallocated to.

   Return value is nonzero if reload failed
   and we must not do any more for this function.  */

int
reload (first, global, dumpfile)
     rtx first;
     int global;
     FILE *dumpfile;
{
  register int class;
  register int i;
  register rtx insn;
  register struct elim_table *ep;

  int something_changed;
  int something_needs_reloads;
  int something_needs_elimination;
  int new_basic_block_needs;
  enum reg_class caller_save_spill_class = NO_REGS;
  int caller_save_group_size = 1;

  /* Nonzero means we couldn't get enough spill regs.  */
  int failure = 0;

  /* The basic block number currently being processed for INSN.  */
  int this_block;

  /* Make sure even insns with volatile mem refs are recognizable.  */
  init_recog ();

  /* Enable find_equiv_reg to distinguish insns made by reload.  */
  reload_first_uid = get_max_uid ();

  for (i = 0; i < N_REG_CLASSES; i++)
    basic_block_needs[i] = 0;

#ifdef SECONDARY_MEMORY_NEEDED
  /* Initialize the secondary memory table.  */
  clear_secondary_mem ();
#endif

  /* Remember which hard regs appear explicitly
     before we merge into `regs_ever_live' the ones in which
     pseudo regs have been allocated.  */
  bcopy (regs_ever_live, regs_explicitly_used, sizeof regs_ever_live);

  /* We don't have a stack slot for any spill reg yet.  */
  bzero (spill_stack_slot, sizeof spill_stack_slot);
  bzero (spill_stack_slot_width, sizeof spill_stack_slot_width);

  /* Initialize the save area information for caller-save, in case some
     are needed.  */
  init_save_areas ();

  /* Compute which hard registers are now in use
     as homes for pseudo registers.
     This is done here rather than (eg) in global_alloc
     because this point is reached even if not optimizing.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    mark_home_live (i);

  /* Make sure that the last insn in the chain
     is not something that needs reloading.  */
  emit_note (NULL_PTR, NOTE_INSN_DELETED);

  /* Find all the pseudo registers that didn't get hard regs
     but do have known equivalent constants or memory slots.
     These include parameters (known equivalent to parameter slots)
     and cse'd or loop-moved constant memory addresses.

     Record constant equivalents in reg_equiv_constant
     so they will be substituted by find_reloads.
     Record memory equivalents in reg_mem_equiv so they can
     be substituted eventually by altering the REG-rtx's.  */

  reg_equiv_constant = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_constant, max_regno * sizeof (rtx));
  reg_equiv_memory_loc = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_memory_loc, max_regno * sizeof (rtx));
  reg_equiv_mem = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_mem, max_regno * sizeof (rtx));
  reg_equiv_init = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_init, max_regno * sizeof (rtx));
  reg_equiv_address = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_address, max_regno * sizeof (rtx));
  reg_max_ref_width = (int *) alloca (max_regno * sizeof (int));
  bzero (reg_max_ref_width, max_regno * sizeof (int));

  /* Look for REG_EQUIV notes; record what each pseudo is equivalent to.
     Also find all paradoxical subregs
     and find largest such for each pseudo.  */

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      rtx set = single_set (insn);

      if (set != 0 && GET_CODE (SET_DEST (set)) == REG)
	{
	  rtx note = find_reg_note (insn, REG_EQUIV, NULL_RTX);
	  if (note
#ifdef LEGITIMATE_PIC_OPERAND_P
	      && (! CONSTANT_P (XEXP (note, 0)) || ! flag_pic
		  || LEGITIMATE_PIC_OPERAND_P (XEXP (note, 0)))
#endif
	      )
	    {
	      rtx x = XEXP (note, 0);
	      i = REGNO (SET_DEST (set));
	      if (i > LAST_VIRTUAL_REGISTER)
		{
		  if (GET_CODE (x) == MEM)
		    reg_equiv_memory_loc[i] = x;
		  else if (CONSTANT_P (x))
		    {
		      if (LEGITIMATE_CONSTANT_P (x))
			reg_equiv_constant[i] = x;
		      else
			reg_equiv_memory_loc[i]
			  = force_const_mem (GET_MODE (SET_DEST (set)), x);
		    }
		  else
		    continue;

		  /* If this register is being made equivalent to a MEM
		     and the MEM is not SET_SRC, the equivalencing insn
		     is one with the MEM as a SET_DEST and it occurs later.
		     So don't mark this insn now.  */
		  if (GET_CODE (x) != MEM
		      || rtx_equal_p (SET_SRC (set), x))
		    reg_equiv_init[i] = insn;
		}
	    }
	}

      /* If this insn is setting a MEM from a register equivalent to it,
	 this is the equivalencing insn.  */
      else if (set && GET_CODE (SET_DEST (set)) == MEM
	       && GET_CODE (SET_SRC (set)) == REG
	       && reg_equiv_memory_loc[REGNO (SET_SRC (set))]
	       && rtx_equal_p (SET_DEST (set),
			       reg_equiv_memory_loc[REGNO (SET_SRC (set))]))
	reg_equiv_init[REGNO (SET_SRC (set))] = insn;

      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	scan_paradoxical_subregs (PATTERN (insn));
    }

  /* Does this function require a frame pointer?  */

  frame_pointer_needed = (! flag_omit_frame_pointer
#ifdef EXIT_IGNORE_STACK
			  /* ?? If EXIT_IGNORE_STACK is set, we will not save
			     and restore sp for alloca.  So we can't eliminate
			     the frame pointer in that case.  At some point,
			     we should improve this by emitting the
			     sp-adjusting insns for this case.  */
			  || (current_function_calls_alloca
			      && EXIT_IGNORE_STACK)
#endif
			  || FRAME_POINTER_REQUIRED);

  num_eliminable = 0;

  /* Initialize the table of registers to eliminate.  The way we do this
     depends on how the eliminable registers were defined.  */
#ifdef ELIMINABLE_REGS
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      ep->can_eliminate = ep->can_eliminate_previous
	= (CAN_ELIMINATE (ep->from, ep->to)
	   && (ep->from != FRAME_POINTER_REGNUM || ! frame_pointer_needed));
    }
#else
  reg_eliminate[0].can_eliminate = reg_eliminate[0].can_eliminate_previous
    = ! frame_pointer_needed;
#endif

  /* Count the number of eliminable registers and build the FROM and TO
     REG rtx's.  Note that code in gen_rtx will cause, e.g.,
     gen_rtx (REG, Pmode, STACK_POINTER_REGNUM) to equal stack_pointer_rtx.
     We depend on this.  */
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      num_eliminable += ep->can_eliminate;
      ep->from_rtx = gen_rtx (REG, Pmode, ep->from);
      ep->to_rtx = gen_rtx (REG, Pmode, ep->to);
    }

  num_labels = max_label_num () - get_first_label_num ();

  /* Allocate the tables used to store offset information at labels.  */
  offsets_known_at = (char *) alloca (num_labels);
  offsets_at
    = (int (*)[NUM_ELIMINABLE_REGS])
      alloca (num_labels * NUM_ELIMINABLE_REGS * sizeof (int));

  offsets_known_at -= get_first_label_num ();
  offsets_at -= get_first_label_num ();

  /* Alter each pseudo-reg rtx to contain its hard reg number.
     Assign stack slots to the pseudos that lack hard regs or equivalents.
     Do not touch virtual registers.  */

  for (i = LAST_VIRTUAL_REGISTER + 1; i < max_regno; i++)
    alter_reg (i, -1);

  /* Round size of stack frame to BIGGEST_ALIGNMENT.  This must be done here
     because the stack size may be a part of the offset computation for
     register elimination.   */
  assign_stack_local (BLKmode, 0, 0);

  /* If we have some registers we think can be eliminated, scan all insns to
     see if there is an insn that sets one of these registers to something
     other than itself plus a constant.  If so, the register cannot be
     eliminated.  Doing this scan here eliminates an extra pass through the
     main reload loop in the most common case where register elimination
     cannot be done.  */
  for (insn = first; insn && num_eliminable; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	|| GET_CODE (insn) == CALL_INSN)
      note_stores (PATTERN (insn), mark_not_eliminable);

#ifndef REGISTER_CONSTRAINTS
  /* If all the pseudo regs have hard regs,
     except for those that are never referenced,
     we know that no reloads are needed.  */
  /* But that is not true if there are register constraints, since
     in that case some pseudos might be in the wrong kind of hard reg.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] == -1 && reg_n_refs[i] != 0)
      break;

  if (i == max_regno && num_eliminable == 0 && ! caller_save_needed)
    return;
#endif

  /* Compute the order of preference for hard registers to spill.
     Store them by decreasing preference in potential_reload_regs.  */

  order_regs_for_reload ();

  /* So far, no hard regs have been spilled.  */
  n_spills = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    spill_reg_order[i] = -1;

  /* On most machines, we can't use any register explicitly used in the
     rtl as a spill register.  But on some, we have to.  Those will have
     taken care to keep the life of hard regs as short as possible.  */

#ifdef SMALL_REGISTER_CLASSES
  CLEAR_HARD_REG_SET (forbidden_regs);
#else
  COPY_HARD_REG_SET (forbidden_regs, bad_spill_regs);
#endif

  /* Spill any hard regs that we know we can't eliminate.  */
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    if (! ep->can_eliminate)
      {
	spill_hard_reg (ep->from, global, dumpfile, 1);
	regs_ever_live[ep->from] = 1;
      }

  if (global)
    for (i = 0; i < N_REG_CLASSES; i++)
      {
	basic_block_needs[i] = (char *)alloca (n_basic_blocks);
	bzero (basic_block_needs[i], n_basic_blocks);
      }

  /* From now on, we need to emit any moves without making new pseudos.  */
  reload_in_progress = 1;

  /* This loop scans the entire function each go-round
     and repeats until one repetition spills no additional hard regs.  */

  /* This flag is set when a pseudo reg is spilled,
     to require another pass.  Note that getting an additional reload
     reg does not necessarily imply any pseudo reg was spilled;
     sometimes we find a reload reg that no pseudo reg was allocated in.  */
  something_changed = 1;
  /* This flag is set if there are any insns that require reloading.  */
  something_needs_reloads = 0;
  /* This flag is set if there are any insns that require register
     eliminations.  */
  something_needs_elimination = 0;
  while (something_changed)
    {
      rtx after_call = 0;

      /* For each class, number of reload regs needed in that class.
	 This is the maximum over all insns of the needs in that class
	 of the individual insn.  */
      int max_needs[N_REG_CLASSES];
      /* For each class, size of group of consecutive regs
	 that is needed for the reloads of this class.  */
      int group_size[N_REG_CLASSES];
      /* For each class, max number of consecutive groups needed.
	 (Each group contains group_size[CLASS] consecutive registers.)  */
      int max_groups[N_REG_CLASSES];
      /* For each class, max number needed of regs that don't belong
	 to any of the groups.  */
      int max_nongroups[N_REG_CLASSES];
      /* For each class, the machine mode which requires consecutive
	 groups of regs of that class.
	 If two different modes ever require groups of one class,
	 they must be the same size and equally restrictive for that class,
	 otherwise we can't handle the complexity.  */
      enum machine_mode group_mode[N_REG_CLASSES];
      /* Record the insn where each maximum need is first found.  */
      rtx max_needs_insn[N_REG_CLASSES];
      rtx max_groups_insn[N_REG_CLASSES];
      rtx max_nongroups_insn[N_REG_CLASSES];
      rtx x;
      int starting_frame_size = get_frame_size ();
      static char *reg_class_names[] = REG_CLASS_NAMES;

      something_changed = 0;
      bzero (max_needs, sizeof max_needs);
      bzero (max_groups, sizeof max_groups);
      bzero (max_nongroups, sizeof max_nongroups);
      bzero (max_needs_insn, sizeof max_needs_insn);
      bzero (max_groups_insn, sizeof max_groups_insn);
      bzero (max_nongroups_insn, sizeof max_nongroups_insn);
      bzero (group_size, sizeof group_size);
      for (i = 0; i < N_REG_CLASSES; i++)
	group_mode[i] = VOIDmode;

      /* Keep track of which basic blocks are needing the reloads.  */
      this_block = 0;

      /* Remember whether any element of basic_block_needs
	 changes from 0 to 1 in this pass.  */
      new_basic_block_needs = 0;

      /* Reset all offsets on eliminable registers to their initial values.  */
#ifdef ELIMINABLE_REGS
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	{
	  INITIAL_ELIMINATION_OFFSET (ep->from, ep->to, ep->initial_offset);
	  ep->previous_offset = ep->offset
	    = ep->max_offset = ep->initial_offset;
	}
#else
#ifdef INITIAL_FRAME_POINTER_OFFSET
      INITIAL_FRAME_POINTER_OFFSET (reg_eliminate[0].initial_offset);
#else
      if (!FRAME_POINTER_REQUIRED)
	abort ();
      reg_eliminate[0].initial_offset = 0;
#endif
      reg_eliminate[0].previous_offset = reg_eliminate[0].max_offset
	= reg_eliminate[0].offset = reg_eliminate[0].initial_offset;
#endif

      num_not_at_initial_offset = 0;

      bzero (&offsets_known_at[get_first_label_num ()], num_labels);

      /* Set a known offset for each forced label to be at the initial offset
	 of each elimination.  We do this because we assume that all
	 computed jumps occur from a location where each elimination is
	 at its initial offset.  */

      for (x = forced_labels; x; x = XEXP (x, 1))
	if (XEXP (x, 0))
	  set_label_offsets (XEXP (x, 0), NULL_RTX, 1);

      /* For each pseudo register that has an equivalent location defined,
	 try to eliminate any eliminable registers (such as the frame pointer)
	 assuming initial offsets for the replacement register, which
	 is the normal case.

	 If the resulting location is directly addressable, substitute
	 the MEM we just got directly for the old REG.

	 If it is not addressable but is a constant or the sum of a hard reg
	 and constant, it is probably not addressable because the constant is
	 out of range, in that case record the address; we will generate
	 hairy code to compute the address in a register each time it is
	 needed.

	 If the location is not addressable, but does not have one of the
	 above forms, assign a stack slot.  We have to do this to avoid the
	 potential of producing lots of reloads if, e.g., a location involves
	 a pseudo that didn't get a hard register and has an equivalent memory
	 location that also involves a pseudo that didn't get a hard register.

	 Perhaps at some point we will improve reload_when_needed handling
	 so this problem goes away.  But that's very hairy.  */

      for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
	if (reg_renumber[i] < 0 && reg_equiv_memory_loc[i])
	  {
	    rtx x = eliminate_regs (reg_equiv_memory_loc[i], 0, NULL_RTX);

	    if (strict_memory_address_p (GET_MODE (regno_reg_rtx[i]),
					 XEXP (x, 0)))
	      reg_equiv_mem[i] = x, reg_equiv_address[i] = 0;
	    else if (CONSTANT_P (XEXP (x, 0))
		     || (GET_CODE (XEXP (x, 0)) == PLUS
			 && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
			 && (REGNO (XEXP (XEXP (x, 0), 0))
			     < FIRST_PSEUDO_REGISTER)
			 && CONSTANT_P (XEXP (XEXP (x, 0), 1))))
	      reg_equiv_address[i] = XEXP (x, 0), reg_equiv_mem[i] = 0;
	    else
	      {
		/* Make a new stack slot.  Then indicate that something
		   changed so we go back and recompute offsets for
		   eliminable registers because the allocation of memory
		   below might change some offset.  reg_equiv_{mem,address}
		   will be set up for this pseudo on the next pass around
		   the loop.  */
		reg_equiv_memory_loc[i] = 0;
		reg_equiv_init[i] = 0;
		alter_reg (i, -1);
		something_changed = 1;
	      }
	  }

      /* If we allocated another pseudo to the stack, redo elimination
	 bookkeeping.  */
      if (something_changed)
	continue;

      /* If caller-saves needs a group, initialize the group to include
	 the size and mode required for caller-saves.  */

      if (caller_save_group_size > 1)
	{
	  group_mode[(int) caller_save_spill_class] = Pmode;
	  group_size[(int) caller_save_spill_class] = caller_save_group_size;
	}

      /* Compute the most additional registers needed by any instruction.
	 Collect information separately for each class of regs.  */

      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  if (global && this_block + 1 < n_basic_blocks
	      && insn == basic_block_head[this_block+1])
	    ++this_block;

	  /* If this is a label, a JUMP_INSN, or has REG_NOTES (which
	     might include REG_LABEL), we need to see what effects this
	     has on the known offsets at labels.  */

	  if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == JUMP_INSN
	      || (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && REG_NOTES (insn) != 0))
	    set_label_offsets (insn, insn, 0);

	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      /* Nonzero means don't use a reload reg that overlaps
		 the place where a function value can be returned.  */
	      rtx avoid_return_reg = 0;

	      rtx old_body = PATTERN (insn);
	      int old_code = INSN_CODE (insn);
 	      rtx old_notes = REG_NOTES (insn);
	      int did_elimination = 0;

	      /* Initially, count RELOAD_OTHER reloads.
		 Later, merge in the other kinds.  */
	      int insn_needs[N_REG_CLASSES];
	      int insn_groups[N_REG_CLASSES];
	      int insn_total_groups = 0;

	      /* Count RELOAD_FOR_INPUT_RELOAD_ADDRESS reloads.  */
	      int insn_needs_for_inputs[N_REG_CLASSES];
	      int insn_groups_for_inputs[N_REG_CLASSES];
	      int insn_total_groups_for_inputs = 0;

	      /* Count RELOAD_FOR_OUTPUT_RELOAD_ADDRESS reloads.  */
	      int insn_needs_for_outputs[N_REG_CLASSES];
	      int insn_groups_for_outputs[N_REG_CLASSES];
	      int insn_total_groups_for_outputs = 0;

	      /* Count RELOAD_FOR_OPERAND_ADDRESS reloads.  */
	      int insn_needs_for_operands[N_REG_CLASSES];
	      int insn_groups_for_operands[N_REG_CLASSES];
	      int insn_total_groups_for_operands = 0;

#if 0  /* This wouldn't work nowadays, since optimize_bit_field
	  looks for non-strict memory addresses.  */
	      /* Optimization: a bit-field instruction whose field
		 happens to be a byte or halfword in memory
		 can be changed to a move instruction.  */

	      if (GET_CODE (PATTERN (insn)) == SET)
		{
		  rtx dest = SET_DEST (PATTERN (insn));
		  rtx src = SET_SRC (PATTERN (insn));

		  if (GET_CODE (dest) == ZERO_EXTRACT
		      || GET_CODE (dest) == SIGN_EXTRACT)
		    optimize_bit_field (PATTERN (insn), insn, reg_equiv_mem);
		  if (GET_CODE (src) == ZERO_EXTRACT
		      || GET_CODE (src) == SIGN_EXTRACT)
		    optimize_bit_field (PATTERN (insn), insn, reg_equiv_mem);
		}
#endif

	      /* If needed, eliminate any eliminable registers.  */
	      if (num_eliminable)
		did_elimination = eliminate_regs_in_insn (insn, 0);

#ifdef SMALL_REGISTER_CLASSES
	      /* Set avoid_return_reg if this is an insn
		 that might use the value of a function call.  */
	      if (GET_CODE (insn) == CALL_INSN)
		{
		  if (GET_CODE (PATTERN (insn)) == SET)
		    after_call = SET_DEST (PATTERN (insn));
		  else if (GET_CODE (PATTERN (insn)) == PARALLEL
			   && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
		    after_call = SET_DEST (XVECEXP (PATTERN (insn), 0, 0));
		  else
		    after_call = 0;
		}
	      else if (after_call != 0
		       && !(GET_CODE (PATTERN (insn)) == SET
			    && SET_DEST (PATTERN (insn)) == stack_pointer_rtx))
		{
		  if (reg_mentioned_p (after_call, PATTERN (insn)))
		    avoid_return_reg = after_call;
		  after_call = 0;
		}
#endif /* SMALL_REGISTER_CLASSES */

	      /* Analyze the instruction.  */
	      find_reloads (insn, 0, spill_indirect_levels, global,
			    spill_reg_order);

	      /* Remember for later shortcuts which insns had any reloads or
		 register eliminations.

		 One might think that it would be worthwhile to mark insns
		 that need register replacements but not reloads, but this is
		 not safe because find_reloads may do some manipulation of
		 the insn (such as swapping commutative operands), which would
		 be lost when we restore the old pattern after register
		 replacement.  So the actions of find_reloads must be redone in
		 subsequent passes or in reload_as_needed.

		 However, it is safe to mark insns that need reloads
		 but not register replacement.  */

	      PUT_MODE (insn, (did_elimination ? QImode
			       : n_reloads ? HImode
			       : VOIDmode));

	      /* Discard any register replacements done.  */
	      if (did_elimination)
		{
		  obstack_free (&reload_obstack, reload_firstobj);
		  PATTERN (insn) = old_body;
		  INSN_CODE (insn) = old_code;
 		  REG_NOTES (insn) = old_notes;
		  something_needs_elimination = 1;
		}

	      /* If this insn has no reloads, we need not do anything except
		 in the case of a CALL_INSN when we have caller-saves and
		 caller-save needs reloads.  */

	      if (n_reloads == 0
		  && ! (GET_CODE (insn) == CALL_INSN
			&& caller_save_spill_class != NO_REGS))
		continue;

	      something_needs_reloads = 1;

	      for (i = 0; i < N_REG_CLASSES; i++)
		{
		  insn_needs[i] = 0, insn_groups[i] = 0;
		  insn_needs_for_inputs[i] = 0, insn_groups_for_inputs[i] = 0;
		  insn_needs_for_outputs[i] = 0, insn_groups_for_outputs[i] = 0;
		  insn_needs_for_operands[i] = 0, insn_groups_for_operands[i] = 0;
		}

	      /* Count each reload once in every class
		 containing the reload's own class.  */

	      for (i = 0; i < n_reloads; i++)
		{
		  register enum reg_class *p;
		  enum reg_class class = reload_reg_class[i];
		  int size;
		  enum machine_mode mode;
		  int *this_groups;
		  int *this_needs;
		  int *this_total_groups;

		  /* Don't count the dummy reloads, for which one of the
		     regs mentioned in the insn can be used for reloading.
		     Don't count optional reloads.
		     Don't count reloads that got combined with others.  */
		  if (reload_reg_rtx[i] != 0
		      || reload_optional[i] != 0
		      || (reload_out[i] == 0 && reload_in[i] == 0
			  && ! reload_secondary_p[i]))
  		    continue;

		  /* Show that a reload register of this class is needed
		     in this basic block.  We do not use insn_needs and
		     insn_groups because they are overly conservative for
		     this purpose.  */
		  if (global && ! basic_block_needs[(int) class][this_block])
		    {
		      basic_block_needs[(int) class][this_block] = 1;
		      new_basic_block_needs = 1;
		    }

		  /* Decide which time-of-use to count this reload for.  */
		  switch (reload_when_needed[i])
		    {
		    case RELOAD_OTHER:
		    case RELOAD_FOR_OUTPUT:
		    case RELOAD_FOR_INPUT:
		      this_needs = insn_needs;
		      this_groups = insn_groups;
		      this_total_groups = &insn_total_groups;
		      break;

		    case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
		      this_needs = insn_needs_for_inputs;
		      this_groups = insn_groups_for_inputs;
		      this_total_groups = &insn_total_groups_for_inputs;
		      break;

		    case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
		      this_needs = insn_needs_for_outputs;
		      this_groups = insn_groups_for_outputs;
		      this_total_groups = &insn_total_groups_for_outputs;
		      break;

		    case RELOAD_FOR_OPERAND_ADDRESS:
		      this_needs = insn_needs_for_operands;
		      this_groups = insn_groups_for_operands;
		      this_total_groups = &insn_total_groups_for_operands;
		      break;
		    }

		  mode = reload_inmode[i];
		  if (GET_MODE_SIZE (reload_outmode[i]) > GET_MODE_SIZE (mode))
		    mode = reload_outmode[i];
		  size = CLASS_MAX_NREGS (class, mode);
		  if (size > 1)
		    {
		      enum machine_mode other_mode, allocate_mode;

		      /* Count number of groups needed separately from
			 number of individual regs needed.  */
		      this_groups[(int) class]++;
		      p = reg_class_superclasses[(int) class];
		      while (*p != LIM_REG_CLASSES)
			this_groups[(int) *p++]++;
		      (*this_total_groups)++;

		      /* Record size and mode of a group of this class.  */
		      /* If more than one size group is needed,
			 make all groups the largest needed size.  */
		      if (group_size[(int) class] < size)
			{
			  other_mode = group_mode[(int) class];
			  allocate_mode = mode;

			  group_size[(int) class] = size;
			  group_mode[(int) class] = mode;
			}
		      else
			{
			  other_mode = mode;
			  allocate_mode = group_mode[(int) class];
			}

		      /* Crash if two dissimilar machine modes both need
			 groups of consecutive regs of the same class.  */

		      if (other_mode != VOIDmode
			  && other_mode != allocate_mode
			  && ! modes_equiv_for_class_p (allocate_mode,
							other_mode,
							class))
			abort ();
		    }
		  else if (size == 1)
		    {
		      this_needs[(int) class] += 1;
		      p = reg_class_superclasses[(int) class];
		      while (*p != LIM_REG_CLASSES)
			this_needs[(int) *p++] += 1;
		    }
		  else
		    abort ();
		}

	      /* All reloads have been counted for this insn;
		 now merge the various times of use.
		 This sets insn_needs, etc., to the maximum total number
		 of registers needed at any point in this insn.  */

	      for (i = 0; i < N_REG_CLASSES; i++)
		{
		  int this_max;
		  this_max = insn_needs_for_inputs[i];
		  if (insn_needs_for_outputs[i] > this_max)
		    this_max = insn_needs_for_outputs[i];
		  if (insn_needs_for_operands[i] > this_max)
		    this_max = insn_needs_for_operands[i];
		  insn_needs[i] += this_max;
		  this_max = insn_groups_for_inputs[i];
		  if (insn_groups_for_outputs[i] > this_max)
		    this_max = insn_groups_for_outputs[i];
		  if (insn_groups_for_operands[i] > this_max)
		    this_max = insn_groups_for_operands[i];
		  insn_groups[i] += this_max;
		}

	      insn_total_groups += MAX (insn_total_groups_for_inputs,
					MAX (insn_total_groups_for_outputs,
					     insn_total_groups_for_operands));

	      /* If this is a CALL_INSN and caller-saves will need
		 a spill register, act as if the spill register is
		 needed for this insn.   However, the spill register
		 can be used by any reload of this insn, so we only
		 need do something if no need for that class has
		 been recorded.

		 The assumption that every CALL_INSN will trigger a
		 caller-save is highly conservative, however, the number
		 of cases where caller-saves will need a spill register but
		 a block containing a CALL_INSN won't need a spill register
		 of that class should be quite rare.

		 If a group is needed, the size and mode of the group will
		 have been set up at the beginning of this loop.  */

	      if (GET_CODE (insn) == CALL_INSN
		  && caller_save_spill_class != NO_REGS)
		{
		  int *caller_save_needs
		    = (caller_save_group_size > 1 ? insn_groups : insn_needs);

		  if (caller_save_needs[(int) caller_save_spill_class] == 0)
		    {
		      register enum reg_class *p
			= reg_class_superclasses[(int) caller_save_spill_class];

		      caller_save_needs[(int) caller_save_spill_class]++;

		      while (*p != LIM_REG_CLASSES)
			caller_save_needs[(int) *p++] += 1;
		    }

		  if (caller_save_group_size > 1)
		    insn_total_groups = MAX (insn_total_groups, 1);


                /* Show that this basic block will need a register of
                   this class.  */

                if (global
                    && ! (basic_block_needs[(int) caller_save_spill_class]
                          [this_block]))
                  {
                    basic_block_needs[(int) caller_save_spill_class]
                      [this_block] = 1;
                    new_basic_block_needs = 1;
                  }
		}

#ifdef SMALL_REGISTER_CLASSES
	      /* If this insn stores the value of a function call,
		 and that value is in a register that has been spilled,
		 and if the insn needs a reload in a class
		 that might use that register as the reload register,
		 then add add an extra need in that class.
		 This makes sure we have a register available that does
		 not overlap the return value.  */
	      if (avoid_return_reg)
		{
		  int regno = REGNO (avoid_return_reg);
		  int nregs
		    = HARD_REGNO_NREGS (regno, GET_MODE (avoid_return_reg));
		  int r;
		  int inc_groups = 0;
		  for (r = regno; r < regno + nregs; r++)
		    if (spill_reg_order[r] >= 0)
		      for (i = 0; i < N_REG_CLASSES; i++)
			if (TEST_HARD_REG_BIT (reg_class_contents[i], r))
			  {
			    if (insn_needs[i] > 0)
			      insn_needs[i]++;
			    if (insn_groups[i] > 0
				&& nregs > 1)
			      inc_groups = 1;
			  }
		  if (inc_groups)
		    insn_groups[i]++;
		}
#endif /* SMALL_REGISTER_CLASSES */

	      /* For each class, collect maximum need of any insn.  */

	      for (i = 0; i < N_REG_CLASSES; i++)
		{
		  if (max_needs[i] < insn_needs[i])
		    {
		      max_needs[i] = insn_needs[i];
		      max_needs_insn[i] = insn;
		    }
		  if (max_groups[i] < insn_groups[i])
		    {
		      max_groups[i] = insn_groups[i];
		      max_groups_insn[i] = insn;
		    }
		  if (insn_total_groups > 0)
		    if (max_nongroups[i] < insn_needs[i])
		      {
			max_nongroups[i] = insn_needs[i];
			max_nongroups_insn[i] = insn;
		      }
		}
	    }
	  /* Note that there is a continue statement above.  */
	}

      /* If we allocated any new memory locations, make another pass
	 since it might have changed elimination offsets.  */
      if (starting_frame_size != get_frame_size ())
	something_changed = 1;

      if (dumpfile)
	for (i = 0; i < N_REG_CLASSES; i++)
	  {
	    if (max_needs[i] > 0)
	      fprintf (dumpfile,
			 ";; Need %d reg%s of class %s (for insn %d).\n",
		       max_needs[i], max_needs[i] == 1 ? "" : "s",
		       reg_class_names[i], INSN_UID (max_needs_insn[i]));
	    if (max_nongroups[i] > 0)
	      fprintf (dumpfile,
		       ";; Need %d nongroup reg%s of class %s (for insn %d).\n",
		       max_nongroups[i], max_nongroups[i] == 1 ? "" : "s",
		       reg_class_names[i], INSN_UID (max_nongroups_insn[i]));
	    if (max_groups[i] > 0)
	      fprintf (dumpfile,
		       ";; Need %d group%s (%smode) of class %s (for insn %d).\n",
		       max_groups[i], max_groups[i] == 1 ? "" : "s",
		       mode_name[(int) group_mode[i]],
		       reg_class_names[i], INSN_UID (max_groups_insn[i]));
	  }
			 
      /* If we have caller-saves, set up the save areas and see if caller-save
	 will need a spill register.  */

      if (caller_save_needed
	  && ! setup_save_areas (&something_changed)
	  && caller_save_spill_class  == NO_REGS)
	{
	  /* The class we will need depends on whether the machine
	     supports the sum of two registers for an address; see
	     find_address_reloads for details.  */

	  caller_save_spill_class
	    = double_reg_address_ok ? INDEX_REG_CLASS : BASE_REG_CLASS;
	  caller_save_group_size
	    = CLASS_MAX_NREGS (caller_save_spill_class, Pmode);
	  something_changed = 1;
	}

      /* Now deduct from the needs for the registers already
	 available (already spilled).  */

      CLEAR_HARD_REG_SET (counted_for_groups);
      CLEAR_HARD_REG_SET (counted_for_nongroups);

      /* First find all regs alone in their class
	 and count them (if desired) for non-groups.
	 We would be screwed if a group took the only reg in a class
	 for which a non-group reload is needed.
	 (Note there is still a bug; if a class has 2 regs,
	 both could be stolen by groups and we would lose the same way.
	 With luck, no machine will need a nongroup in a 2-reg class.)  */

      for (i = 0; i < n_spills; i++)
	{
	  register enum reg_class *p;
	  class = (int) REGNO_REG_CLASS (spill_regs[i]);

	  if (reg_class_size[class] == 1 && max_nongroups[class] > 0)
	    {
	      max_needs[class]--;
	      p = reg_class_superclasses[class];
	      while (*p != LIM_REG_CLASSES)
		max_needs[(int) *p++]--;

	      SET_HARD_REG_BIT (counted_for_nongroups, spill_regs[i]);
	      max_nongroups[class]--;
	      p = reg_class_superclasses[class];
	      while (*p != LIM_REG_CLASSES)
		{
		  if (max_nongroups[(int) *p] > 0)
		    SET_HARD_REG_BIT (counted_for_nongroups, spill_regs[i]);
		  max_nongroups[(int) *p++]--;
		}
	    }
	}

      /* Now find all consecutive groups of spilled registers
	 and mark each group off against the need for such groups.
	 But don't count them against ordinary need, yet.  */

      count_possible_groups (group_size, group_mode, max_groups);

      /* Now count all spill regs against the individual need,
	 This includes those counted above for groups,
	 but not those previously counted for nongroups.

	 Those that weren't counted_for_groups can also count against
	 the not-in-group need.  */

      for (i = 0; i < n_spills; i++)
	{
	  register enum reg_class *p;
	  class = (int) REGNO_REG_CLASS (spill_regs[i]);

	  /* Those counted at the beginning shouldn't be counted twice.  */
	  if (! TEST_HARD_REG_BIT (counted_for_nongroups, spill_regs[i]))
	    {
	      max_needs[class]--;
	      p = reg_class_superclasses[class];
	      while (*p != LIM_REG_CLASSES)
		max_needs[(int) *p++]--;

	      if (! TEST_HARD_REG_BIT (counted_for_groups, spill_regs[i]))
		{
		  if (max_nongroups[class] > 0)
		    SET_HARD_REG_BIT (counted_for_nongroups, spill_regs[i]);
		  max_nongroups[class]--;
		  p = reg_class_superclasses[class];
		  while (*p != LIM_REG_CLASSES)
		    {
		      if (max_nongroups[(int) *p] > 0)
			SET_HARD_REG_BIT (counted_for_nongroups,
					  spill_regs[i]);
		      max_nongroups[(int) *p++]--;
		    }
		}
	    }
	}

      /* See if anything that happened changes which eliminations are valid.
	 For example, on the Sparc, whether or not the frame pointer can
	 be eliminated can depend on what registers have been used.  We need
	 not check some conditions again (such as flag_omit_frame_pointer)
	 since they can't have changed.  */

      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if ((ep->from == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED)
#ifdef ELIMINABLE_REGS
	    || ! CAN_ELIMINATE (ep->from, ep->to)
#endif
	    )
	  ep->can_eliminate = 0;

      /* Look for the case where we have discovered that we can't replace
	 register A with register B and that means that we will now be
	 trying to replace register A with register C.  This means we can
	 no longer replace register C with register B and we need to disable
	 such an elimination, if it exists.  This occurs often with A == ap,
	 B == sp, and C == fp.  */

      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	{
	  struct elim_table *op;
	  register int new_to = -1;

	  if (! ep->can_eliminate && ep->can_eliminate_previous)
	    {
	      /* Find the current elimination for ep->from, if there is a
		 new one.  */
	      for (op = reg_eliminate;
		   op < &reg_eliminate[NUM_ELIMINABLE_REGS]; op++)
		if (op->from == ep->from && op->can_eliminate)
		  {
		    new_to = op->to;
		    break;
		  }

	      /* See if there is an elimination of NEW_TO -> EP->TO.  If so,
		 disable it.  */
	      for (op = reg_eliminate;
		   op < &reg_eliminate[NUM_ELIMINABLE_REGS]; op++)
		if (op->from == new_to && op->to == ep->to)
		  op->can_eliminate = 0;
	    }
	}

      /* See if any registers that we thought we could eliminate the previous
	 time are no longer eliminable.  If so, something has changed and we
	 must spill the register.  Also, recompute the number of eliminable
	 registers and see if the frame pointer is needed; it is if there is
	 no elimination of the frame pointer that we can perform.  */

      frame_pointer_needed = 1;
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	{
	  if (ep->can_eliminate && ep->from == FRAME_POINTER_REGNUM)
	    frame_pointer_needed = 0;

	  if (! ep->can_eliminate && ep->can_eliminate_previous)
	    {
	      ep->can_eliminate_previous = 0;
	      spill_hard_reg (ep->from, global, dumpfile, 1);
	      regs_ever_live[ep->from] = 1;
	      something_changed = 1;
	      num_eliminable--;
	    }
	}

      /* If all needs are met, we win.  */

      for (i = 0; i < N_REG_CLASSES; i++)
	if (max_needs[i] > 0 || max_groups[i] > 0 || max_nongroups[i] > 0)
	  break;
      if (i == N_REG_CLASSES && !new_basic_block_needs && ! something_changed)
	break;

      /* Not all needs are met; must spill more hard regs.  */

      /* If any element of basic_block_needs changed from 0 to 1,
	 re-spill all the regs already spilled.  This may spill
	 additional pseudos that didn't spill before.  */

      if (new_basic_block_needs)
	for (i = 0; i < n_spills; i++)
	  something_changed
	    |= spill_hard_reg (spill_regs[i], global, dumpfile, 0);

      /* Now find more reload regs to satisfy the remaining need
	 Do it by ascending class number, since otherwise a reg
	 might be spilled for a big class and might fail to count
	 for a smaller class even though it belongs to that class.

	 Count spilled regs in `spills', and add entries to
	 `spill_regs' and `spill_reg_order'.

	 ??? Note there is a problem here.
	 When there is a need for a group in a high-numbered class,
	 and also need for non-group regs that come from a lower class,
	 the non-group regs are chosen first.  If there aren't many regs,
	 they might leave no room for a group.

	 This was happening on the 386.  To fix it, we added the code
	 that calls possible_group_p, so that the lower class won't
	 break up the last possible group.

	 Really fixing the problem would require changes above
	 in counting the regs already spilled, and in choose_reload_regs.
	 It might be hard to avoid introducing bugs there.  */

      for (class = 0; class < N_REG_CLASSES; class++)
	{
	  /* First get the groups of registers.
	     If we got single registers first, we might fragment
	     possible groups.  */
	  while (max_groups[class] > 0)
	    {
	      /* If any single spilled regs happen to form groups,
		 count them now.  Maybe we don't really need
		 to spill another group.  */
	      count_possible_groups (group_size, group_mode, max_groups);

	      /* Groups of size 2 (the only groups used on most machines)
		 are treated specially.  */
	      if (group_size[class] == 2)
		{
		  /* First, look for a register that will complete a group.  */
		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    {
		      int j = potential_reload_regs[i];
		      int other;
		      if (j >= 0 && ! TEST_HARD_REG_BIT (bad_spill_regs, j)
			  &&
			  ((j > 0 && (other = j - 1, spill_reg_order[other] >= 0)
			    && TEST_HARD_REG_BIT (reg_class_contents[class], j)
			    && TEST_HARD_REG_BIT (reg_class_contents[class], other)
			    && HARD_REGNO_MODE_OK (other, group_mode[class])
			    && ! TEST_HARD_REG_BIT (counted_for_nongroups,
						    other)
			    /* We don't want one part of another group.
			       We could get "two groups" that overlap!  */
			    && ! TEST_HARD_REG_BIT (counted_for_groups, other))
			   ||
			   (j < FIRST_PSEUDO_REGISTER - 1
			    && (other = j + 1, spill_reg_order[other] >= 0)
			    && TEST_HARD_REG_BIT (reg_class_contents[class], j)
			    && TEST_HARD_REG_BIT (reg_class_contents[class], other)
			    && HARD_REGNO_MODE_OK (j, group_mode[class])
			    && ! TEST_HARD_REG_BIT (counted_for_nongroups,
						    other)
			    && ! TEST_HARD_REG_BIT (counted_for_groups,
						    other))))
			{
			  register enum reg_class *p;

			  /* We have found one that will complete a group,
			     so count off one group as provided.  */
			  max_groups[class]--;
			  p = reg_class_superclasses[class];
			  while (*p != LIM_REG_CLASSES)
			    max_groups[(int) *p++]--;

			  /* Indicate both these regs are part of a group.  */
			  SET_HARD_REG_BIT (counted_for_groups, j);
			  SET_HARD_REG_BIT (counted_for_groups, other);
			  break;
			}
		    }
		  /* We can't complete a group, so start one.  */
		  if (i == FIRST_PSEUDO_REGISTER)
		    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		      {
			int j = potential_reload_regs[i];
			if (j >= 0 && j + 1 < FIRST_PSEUDO_REGISTER
			    && spill_reg_order[j] < 0 && spill_reg_order[j + 1] < 0
			    && TEST_HARD_REG_BIT (reg_class_contents[class], j)
			    && TEST_HARD_REG_BIT (reg_class_contents[class], j + 1)
			    && HARD_REGNO_MODE_OK (j, group_mode[class])
			    && ! TEST_HARD_REG_BIT (counted_for_nongroups,
						    j + 1))
			  break;
		      }

		  /* I should be the index in potential_reload_regs
		     of the new reload reg we have found.  */

		  if (i >= FIRST_PSEUDO_REGISTER)
		    {
		      /* There are no groups left to spill.  */
		      spill_failure (max_groups_insn[class]);
		      failure = 1;
		      goto failed;
		    }
		  else
		    something_changed
		      |= new_spill_reg (i, class, max_needs, NULL_PTR,
					global, dumpfile);
		}
	      else
		{
		  /* For groups of more than 2 registers,
		     look for a sufficient sequence of unspilled registers,
		     and spill them all at once.  */
		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    {
		      int j = potential_reload_regs[i];
		      int k;
		      if (j >= 0
			  && j + group_size[class] <= FIRST_PSEUDO_REGISTER
			  && HARD_REGNO_MODE_OK (j, group_mode[class]))
			{
			  /* Check each reg in the sequence.  */
			  for (k = 0; k < group_size[class]; k++)
			    if (! (spill_reg_order[j + k] < 0
				   && ! TEST_HARD_REG_BIT (bad_spill_regs, j + k)
				   && TEST_HARD_REG_BIT (reg_class_contents[class], j + k)))
			      break;
			  /* We got a full sequence, so spill them all.  */
			  if (k == group_size[class])
			    {
			      register enum reg_class *p;
			      for (k = 0; k < group_size[class]; k++)
				{
				  int idx;
				  SET_HARD_REG_BIT (counted_for_groups, j + k);
				  for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
				    if (potential_reload_regs[idx] == j + k)
				      break;
				  something_changed
				    |= new_spill_reg (idx, class,
						      max_needs, NULL_PTR,
						      global, dumpfile);
				}

			      /* We have found one that will complete a group,
				 so count off one group as provided.  */
			      max_groups[class]--;
			      p = reg_class_superclasses[class];
			      while (*p != LIM_REG_CLASSES)
				max_groups[(int) *p++]--;

			      break;
			    }
			}
		    }
		  /* We couldn't find any registers for this reload.
		     Avoid going into an infinite loop.  */
		  if (i >= FIRST_PSEUDO_REGISTER)
		    {
		      /* There are no groups left.  */
		      spill_failure (max_groups_insn[class]);
		      failure = 1;
		      goto failed;
		    }
		}
	    }

	  /* Now similarly satisfy all need for single registers.  */

	  while (max_needs[class] > 0 || max_nongroups[class] > 0)
	    {
	      /* Consider the potential reload regs that aren't
		 yet in use as reload regs, in order of preference.
		 Find the most preferred one that's in this class.  */

	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (potential_reload_regs[i] >= 0
		    && TEST_HARD_REG_BIT (reg_class_contents[class],
					  potential_reload_regs[i])
		    /* If this reg will not be available for groups,
		       pick one that does not foreclose possible groups.
		       This is a kludge, and not very general,
		       but it should be sufficient to make the 386 work,
		       and the problem should not occur on machines with
		       more registers.  */
		    && (max_nongroups[class] == 0
			|| possible_group_p (potential_reload_regs[i], max_groups)))
		  break;

	      /* If we couldn't get a register, try to get one even if we
		 might foreclose possible groups.  This may cause problems
		 later, but that's better than aborting now, since it is
		 possible that we will, in fact, be able to form the needed
		 group even with this allocation.  */

	      if (i >= FIRST_PSEUDO_REGISTER
		  && (asm_noperands (max_needs[class] > 0
				     ? max_needs_insn[class]
				     : max_nongroups_insn[class])
		      < 0))
		for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		  if (potential_reload_regs[i] >= 0
		      && TEST_HARD_REG_BIT (reg_class_contents[class],
					    potential_reload_regs[i]))
		    break;

	      /* I should be the index in potential_reload_regs
		 of the new reload reg we have found.  */

	      if (i >= FIRST_PSEUDO_REGISTER)
		{
		  /* There are no possible registers left to spill.  */
		  spill_failure (max_needs[class] > 0 ? max_needs_insn[class]
				 : max_nongroups_insn[class]);
		  failure = 1;
		  goto failed;
		}
	      else
		something_changed
		  |= new_spill_reg (i, class, max_needs, max_nongroups,
				    global, dumpfile);
	    }
	}
    }

  /* If global-alloc was run, notify it of any register eliminations we have
     done.  */
  if (global)
    for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
      if (ep->can_eliminate)
	mark_elimination (ep->from, ep->to);

  /* Insert code to save and restore call-clobbered hard regs
     around calls.  Tell if what mode to use so that we will process
     those insns in reload_as_needed if we have to.  */

  if (caller_save_needed)
    save_call_clobbered_regs (num_eliminable ? QImode
			      : caller_save_spill_class != NO_REGS ? HImode
			      : VOIDmode);

  /* If a pseudo has no hard reg, delete the insns that made the equivalence.
     If that insn didn't set the register (i.e., it copied the register to
     memory), just delete that insn instead of the equivalencing insn plus
     anything now dead.  If we call delete_dead_insn on that insn, we may
     delete the insn that actually sets the register if the register die
     there and that is incorrect.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] < 0 && reg_equiv_init[i] != 0
	&& GET_CODE (reg_equiv_init[i]) != NOTE)
      {
	if (reg_set_p (regno_reg_rtx[i], PATTERN (reg_equiv_init[i])))
	  delete_dead_insn (reg_equiv_init[i]);
	else
	  {
	    PUT_CODE (reg_equiv_init[i], NOTE);
	    NOTE_SOURCE_FILE (reg_equiv_init[i]) = 0;
	    NOTE_LINE_NUMBER (reg_equiv_init[i]) = NOTE_INSN_DELETED;
	  }
      }

  /* Use the reload registers where necessary
     by generating move instructions to move the must-be-register
     values into or out of the reload registers.  */

  if (something_needs_reloads || something_needs_elimination
      || (caller_save_needed && num_eliminable)
      || caller_save_spill_class != NO_REGS)
    reload_as_needed (first, global);

  /* If we were able to eliminate the frame pointer, show that it is no
     longer live at the start of any basic block.  If it is live by
     virtue of being in a pseudo, that pseudo will be marked live
     and hence the frame pointer will be known to be live via that
     pseudo.  */

  if (! frame_pointer_needed)
    for (i = 0; i < n_basic_blocks; i++)
      basic_block_live_at_start[i][FRAME_POINTER_REGNUM / REGSET_ELT_BITS]
	&= ~ ((REGSET_ELT_TYPE) 1 << (FRAME_POINTER_REGNUM % REGSET_ELT_BITS));

  reload_in_progress = 0;

  /* Come here (with failure set nonzero) if we can't get enough spill regs
     and we decide not to abort about it.  */
 failed:

  /* Now eliminate all pseudo regs by modifying them into
     their equivalent memory references.
     The REG-rtx's for the pseudos are modified in place,
     so all insns that used to refer to them now refer to memory.

     For a reg that has a reg_equiv_address, all those insns
     were changed by reloading so that no insns refer to it any longer;
     but the DECL_RTL of a variable decl may refer to it,
     and if so this causes the debugging info to mention the variable.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    {
      rtx addr = 0;
      int in_struct = 0;
      if (reg_equiv_mem[i])
	{
	  addr = XEXP (reg_equiv_mem[i], 0);
	  in_struct = MEM_IN_STRUCT_P (reg_equiv_mem[i]);
	}
      if (reg_equiv_address[i])
	addr = reg_equiv_address[i];
      if (addr)
	{
	  if (reg_renumber[i] < 0)
	    {
	      rtx reg = regno_reg_rtx[i];
	      XEXP (reg, 0) = addr;
	      REG_USERVAR_P (reg) = 0;
	      MEM_IN_STRUCT_P (reg) = in_struct;
	      PUT_CODE (reg, MEM);
	    }
	  else if (reg_equiv_mem[i])
	    XEXP (reg_equiv_mem[i], 0) = addr;
	}
    }

#ifdef PRESERVE_DEATH_INFO_REGNO_P
  /* Make a pass over all the insns and remove death notes for things that
     are no longer registers or no longer die in the insn (e.g., an input
     and output pseudo being tied).  */

  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	rtx note, next;

	for (note = REG_NOTES (insn); note; note = next)
	  {
	    next = XEXP (note, 1);
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& (GET_CODE (XEXP (note, 0)) != REG
		    || reg_set_p (XEXP (note, 0), PATTERN (insn))))
	      remove_note (insn, note);
	  }
      }
#endif

  /* Indicate that we no longer have known memory locations or constants.  */
  reg_equiv_constant = 0;
  reg_equiv_memory_loc = 0;

  return failure;
}

/* Nonzero if, after spilling reg REGNO for non-groups,
   it will still be possible to find a group if we still need one.  */

static int
possible_group_p (regno, max_groups)
     int regno;
     int *max_groups;
{
  int i;
  int class = (int) NO_REGS;

  for (i = 0; i < (int) N_REG_CLASSES; i++)
    if (max_groups[i] > 0)
      {
	class = i;
	break;
      }

  if (class == (int) NO_REGS)
    return 1;

  /* Consider each pair of consecutive registers.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER - 1; i++)
    {
      /* Ignore pairs that include reg REGNO.  */
      if (i == regno || i + 1 == regno)
	continue;

      /* Ignore pairs that are outside the class that needs the group.
	 ??? Here we fail to handle the case where two different classes
	 independently need groups.  But this never happens with our
	 current machine descriptions.  */
      if (! (TEST_HARD_REG_BIT (reg_class_contents[class], i)
	     && TEST_HARD_REG_BIT (reg_class_contents[class], i + 1)))
	continue;

      /* A pair of consecutive regs we can still spill does the trick.  */
      if (spill_reg_order[i] < 0 && spill_reg_order[i + 1] < 0
	  && ! TEST_HARD_REG_BIT (bad_spill_regs, i)
	  && ! TEST_HARD_REG_BIT (bad_spill_regs, i + 1))
	return 1;

      /* A pair of one already spilled and one we can spill does it
	 provided the one already spilled is not otherwise reserved.  */
      if (spill_reg_order[i] < 0
	  && ! TEST_HARD_REG_BIT (bad_spill_regs, i)
	  && spill_reg_order[i + 1] >= 0
	  && ! TEST_HARD_REG_BIT (counted_for_groups, i + 1)
	  && ! TEST_HARD_REG_BIT (counted_for_nongroups, i + 1))
	return 1;
      if (spill_reg_order[i + 1] < 0
	  && ! TEST_HARD_REG_BIT (bad_spill_regs, i + 1)
	  && spill_reg_order[i] >= 0
	  && ! TEST_HARD_REG_BIT (counted_for_groups, i)
	  && ! TEST_HARD_REG_BIT (counted_for_nongroups, i))
	return 1;
    }

  return 0;
}

/* Count any groups that can be formed from the registers recently spilled.
   This is done class by class, in order of ascending class number.  */

static void
count_possible_groups (group_size, group_mode, max_groups)
     int *group_size, *max_groups;
     enum machine_mode *group_mode;
{
  int i;
  /* Now find all consecutive groups of spilled registers
     and mark each group off against the need for such groups.
     But don't count them against ordinary need, yet.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    if (group_size[i] > 1)
      {
	char regmask[FIRST_PSEUDO_REGISTER];
	int j;

	bzero (regmask, sizeof regmask);
	/* Make a mask of all the regs that are spill regs in class I.  */
	for (j = 0; j < n_spills; j++)
	  if (TEST_HARD_REG_BIT (reg_class_contents[i], spill_regs[j])
	      && ! TEST_HARD_REG_BIT (counted_for_groups, spill_regs[j])
	      && ! TEST_HARD_REG_BIT (counted_for_nongroups,
				      spill_regs[j]))
	    regmask[spill_regs[j]] = 1;
	/* Find each consecutive group of them.  */
	for (j = 0; j < FIRST_PSEUDO_REGISTER && max_groups[i] > 0; j++)
	  if (regmask[j] && j + group_size[i] <= FIRST_PSEUDO_REGISTER
	      /* Next line in case group-mode for this class
		 demands an even-odd pair.  */
	      && HARD_REGNO_MODE_OK (j, group_mode[i]))
	    {
	      int k;
	      for (k = 1; k < group_size[i]; k++)
		if (! regmask[j + k])
		  break;
	      if (k == group_size[i])
		{
		  /* We found a group.  Mark it off against this class's
		     need for groups, and against each superclass too.  */
		  register enum reg_class *p;
		  max_groups[i]--;
		  p = reg_class_superclasses[i];
		  while (*p != LIM_REG_CLASSES)
		    max_groups[(int) *p++]--;
		  /* Don't count these registers again.  */
		  for (k = 0; k < group_size[i]; k++)
		    SET_HARD_REG_BIT (counted_for_groups, j + k);
		}
	      /* Skip to the last reg in this group.  When j is incremented
		 above, it will then point to the first reg of the next
		 possible group.  */
	      j += k - 1;
	    }
      }

}

/* ALLOCATE_MODE is a register mode that needs to be reloaded.  OTHER_MODE is
   another mode that needs to be reloaded for the same register class CLASS.
   If any reg in CLASS allows ALLOCATE_MODE but not OTHER_MODE, fail.
   ALLOCATE_MODE will never be smaller than OTHER_MODE.

   This code used to also fail if any reg in CLASS allows OTHER_MODE but not
   ALLOCATE_MODE.  This test is unnecessary, because we will never try to put
   something of mode ALLOCATE_MODE into an OTHER_MODE register.  Testing this
   causes unnecessary failures on machines requiring alignment of register
   groups when the two modes are different sizes, because the larger mode has
   more strict alignment rules than the smaller mode.  */

static int
modes_equiv_for_class_p (allocate_mode, other_mode, class)
     enum machine_mode allocate_mode, other_mode;
     enum reg_class class;
{
  register int regno;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (TEST_HARD_REG_BIT (reg_class_contents[(int) class], regno)
	  && HARD_REGNO_MODE_OK (regno, allocate_mode)
	  && ! HARD_REGNO_MODE_OK (regno, other_mode))
	return 0;
    }
  return 1;
}

/* Handle the failure to find a register to spill.
   INSN should be one of the insns which needed this particular spill reg.  */

static void
spill_failure (insn)
     rtx insn;
{
  if (asm_noperands (PATTERN (insn)) >= 0)
    error_for_asm (insn, "`asm' needs too many reloads");
  else
    abort ();
}

/* Add a new register to the tables of available spill-registers
    (as well as spilling all pseudos allocated to the register).
   I is the index of this register in potential_reload_regs.
   CLASS is the regclass whose need is being satisfied.
   MAX_NEEDS and MAX_NONGROUPS are the vectors of needs,
    so that this register can count off against them.
    MAX_NONGROUPS is 0 if this register is part of a group.
   GLOBAL and DUMPFILE are the same as the args that `reload' got.  */

static int
new_spill_reg (i, class, max_needs, max_nongroups, global, dumpfile)
     int i;
     int class;
     int *max_needs;
     int *max_nongroups;
     int global;
     FILE *dumpfile;
{
  register enum reg_class *p;
  int val;
  int regno = potential_reload_regs[i];

  if (i >= FIRST_PSEUDO_REGISTER)
    abort ();	/* Caller failed to find any register.  */

  if (fixed_regs[regno] || TEST_HARD_REG_BIT (forbidden_regs, regno))
    fatal ("fixed or forbidden register was spilled.\n\
This may be due to a compiler bug or to impossible asm statements.");

  /* Make reg REGNO an additional reload reg.  */

  potential_reload_regs[i] = -1;
  spill_regs[n_spills] = regno;
  spill_reg_order[regno] = n_spills;
  if (dumpfile)
    fprintf (dumpfile, "Spilling reg %d.\n", spill_regs[n_spills]);

  /* Clear off the needs we just satisfied.  */

  max_needs[class]--;
  p = reg_class_superclasses[class];
  while (*p != LIM_REG_CLASSES)
    max_needs[(int) *p++]--;

  if (max_nongroups && max_nongroups[class] > 0)
    {
      SET_HARD_REG_BIT (counted_for_nongroups, regno);
      max_nongroups[class]--;
      p = reg_class_superclasses[class];
      while (*p != LIM_REG_CLASSES)
	max_nongroups[(int) *p++]--;
    }

  /* Spill every pseudo reg that was allocated to this reg
     or to something that overlaps this reg.  */

  val = spill_hard_reg (spill_regs[n_spills], global, dumpfile, 0);

  /* If there are some registers still to eliminate and this register
     wasn't ever used before, additional stack space may have to be
     allocated to store this register.  Thus, we may have changed the offset
     between the stack and frame pointers, so mark that something has changed.
     (If new pseudos were spilled, thus requiring more space, VAL would have
     been set non-zero by the call to spill_hard_reg above since additional
     reloads may be needed in that case.

     One might think that we need only set VAL to 1 if this is a call-used
     register.  However, the set of registers that must be saved by the
     prologue is not identical to the call-used set.  For example, the
     register used by the call insn for the return PC is a call-used register,
     but must be saved by the prologue.  */
  if (num_eliminable && ! regs_ever_live[spill_regs[n_spills]])
    val = 1;

  regs_ever_live[spill_regs[n_spills]] = 1;
  n_spills++;

  return val;
}

/* Delete an unneeded INSN and any previous insns who sole purpose is loading
   data that is dead in INSN.  */

static void
delete_dead_insn (insn)
     rtx insn;
{
  rtx prev = prev_real_insn (insn);
  rtx prev_dest;

  /* If the previous insn sets a register that dies in our insn, delete it
     too.  */
  if (prev && GET_CODE (PATTERN (prev)) == SET
      && (prev_dest = SET_DEST (PATTERN (prev)), GET_CODE (prev_dest) == REG)
      && reg_mentioned_p (prev_dest, PATTERN (insn))
      && find_regno_note (insn, REG_DEAD, REGNO (prev_dest)))
    delete_dead_insn (prev);

  PUT_CODE (insn, NOTE);
  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (insn) = 0;
}

/* Modify the home of pseudo-reg I.
   The new home is present in reg_renumber[I].

   FROM_REG may be the hard reg that the pseudo-reg is being spilled from;
   or it may be -1, meaning there is none or it is not relevant.
   This is used so that all pseudos spilled from a given hard reg
   can share one stack slot.  */

static void
alter_reg (i, from_reg)
     register int i;
     int from_reg;
{
  /* When outputting an inline function, this can happen
     for a reg that isn't actually used.  */
  if (regno_reg_rtx[i] == 0)
    return;

  /* If the reg got changed to a MEM at rtl-generation time,
     ignore it.  */
  if (GET_CODE (regno_reg_rtx[i]) != REG)
    return;

  /* Modify the reg-rtx to contain the new hard reg
     number or else to contain its pseudo reg number.  */
  REGNO (regno_reg_rtx[i])
    = reg_renumber[i] >= 0 ? reg_renumber[i] : i;

  /* If we have a pseudo that is needed but has no hard reg or equivalent,
     allocate a stack slot for it.  */

  if (reg_renumber[i] < 0
      && reg_n_refs[i] > 0
      && reg_equiv_constant[i] == 0
      && reg_equiv_memory_loc[i] == 0)
    {
      register rtx x;
      int inherent_size = PSEUDO_REGNO_BYTES (i);
      int total_size = MAX (inherent_size, reg_max_ref_width[i]);
      int adjust = 0;

      /* Each pseudo reg has an inherent size which comes from its own mode,
	 and a total size which provides room for paradoxical subregs
	 which refer to the pseudo reg in wider modes.

	 We can use a slot already allocated if it provides both
	 enough inherent space and enough total space.
	 Otherwise, we allocate a new slot, making sure that it has no less
	 inherent space, and no less total space, then the previous slot.  */
      if (from_reg == -1)
	{
	  /* No known place to spill from => no slot to reuse.  */
	  x = assign_stack_local (GET_MODE (regno_reg_rtx[i]), total_size, -1);
#if BYTES_BIG_ENDIAN
	  /* Cancel the  big-endian correction done in assign_stack_local.
	     Get the address of the beginning of the slot.
	     This is so we can do a big-endian correction unconditionally
	     below.  */
	  adjust = inherent_size - total_size;
#endif
	}
      /* Reuse a stack slot if possible.  */
      else if (spill_stack_slot[from_reg] != 0
	       && spill_stack_slot_width[from_reg] >= total_size
	       && (GET_MODE_SIZE (GET_MODE (spill_stack_slot[from_reg]))
		   >= inherent_size))
	x = spill_stack_slot[from_reg];
      /* Allocate a bigger slot.  */
      else
	{
	  /* Compute maximum size needed, both for inherent size
	     and for total size.  */
	  enum machine_mode mode = GET_MODE (regno_reg_rtx[i]);
	  if (spill_stack_slot[from_reg])
	    {
	      if (GET_MODE_SIZE (GET_MODE (spill_stack_slot[from_reg]))
		  > inherent_size)
		mode = GET_MODE (spill_stack_slot[from_reg]);
	      if (spill_stack_slot_width[from_reg] > total_size)
		total_size = spill_stack_slot_width[from_reg];
	    }
	  /* Make a slot with that size.  */
	  x = assign_stack_local (mode, total_size, -1);
#if BYTES_BIG_ENDIAN
	  /* Cancel the  big-endian correction done in assign_stack_local.
	     Get the address of the beginning of the slot.
	     This is so we can do a big-endian correction unconditionally
	     below.  */
	  adjust = GET_MODE_SIZE (mode) - total_size;
#endif
	  spill_stack_slot[from_reg] = x;
	  spill_stack_slot_width[from_reg] = total_size;
	}

#if BYTES_BIG_ENDIAN
      /* On a big endian machine, the "address" of the slot
	 is the address of the low part that fits its inherent mode.  */
      if (inherent_size < total_size)
	adjust += (total_size - inherent_size);
#endif /* BYTES_BIG_ENDIAN */

      /* If we have any adjustment to make, or if the stack slot is the
	 wrong mode, make a new stack slot.  */
      if (adjust != 0 || GET_MODE (x) != GET_MODE (regno_reg_rtx[i]))
	{
	  x = gen_rtx (MEM, GET_MODE (regno_reg_rtx[i]),
		       plus_constant (XEXP (x, 0), adjust));
	  RTX_UNCHANGING_P (x) = RTX_UNCHANGING_P (regno_reg_rtx[i]);
	}

      /* Save the stack slot for later.   */
      reg_equiv_memory_loc[i] = x;
    }
}

/* Mark the slots in regs_ever_live for the hard regs
   used by pseudo-reg number REGNO.  */

void
mark_home_live (regno)
     int regno;
{
  register int i, lim;
  i = reg_renumber[regno];
  if (i < 0)
    return;
  lim = i + HARD_REGNO_NREGS (i, PSEUDO_REGNO_MODE (regno));
  while (i < lim)
    regs_ever_live[i++] = 1;
}

/* This function handles the tracking of elimination offsets around branches.

   X is a piece of RTL being scanned.

   INSN is the insn that it came from, if any.

   INITIAL_P is non-zero if we are to set the offset to be the initial
   offset and zero if we are setting the offset of the label to be the
   current offset.  */

static void
set_label_offsets (x, insn, initial_p)
     rtx x;
     rtx insn;
     int initial_p;
{
  enum rtx_code code = GET_CODE (x);
  rtx tem;
  int i;
  struct elim_table *p;

  switch (code)
    {
    case LABEL_REF:
      if (LABEL_REF_NONLOCAL_P (x))
	return;

      x = XEXP (x, 0);

      /* ... fall through ... */

    case CODE_LABEL:
      /* If we know nothing about this label, set the desired offsets.  Note
	 that this sets the offset at a label to be the offset before a label
	 if we don't know anything about the label.  This is not correct for
	 the label after a BARRIER, but is the best guess we can make.  If
	 we guessed wrong, we will suppress an elimination that might have
	 been possible had we been able to guess correctly.  */

      if (! offsets_known_at[CODE_LABEL_NUMBER (x)])
	{
	  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
	    offsets_at[CODE_LABEL_NUMBER (x)][i]
	      = (initial_p ? reg_eliminate[i].initial_offset
		 : reg_eliminate[i].offset);
	  offsets_known_at[CODE_LABEL_NUMBER (x)] = 1;
	}

      /* Otherwise, if this is the definition of a label and it is
	 preceded by a BARRIER, set our offsets to the known offset of
	 that label.  */

      else if (x == insn
	       && (tem = prev_nonnote_insn (insn)) != 0
	       && GET_CODE (tem) == BARRIER)
	{
	  num_not_at_initial_offset = 0;
	  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
	    {
	      reg_eliminate[i].offset = reg_eliminate[i].previous_offset
		= offsets_at[CODE_LABEL_NUMBER (x)][i];
	      if (reg_eliminate[i].can_eliminate
		  && (reg_eliminate[i].offset
		      != reg_eliminate[i].initial_offset))
		num_not_at_initial_offset++;
	    }
	}

      else
	/* If neither of the above cases is true, compare each offset
	   with those previously recorded and suppress any eliminations
	   where the offsets disagree.  */

	for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
	  if (offsets_at[CODE_LABEL_NUMBER (x)][i]
	      != (initial_p ? reg_eliminate[i].initial_offset
		  : reg_eliminate[i].offset))
	    reg_eliminate[i].can_eliminate = 0;

      return;

    case JUMP_INSN:
      set_label_offsets (PATTERN (insn), insn, initial_p);

      /* ... fall through ... */

    case INSN:
    case CALL_INSN:
      /* Any labels mentioned in REG_LABEL notes can be branched to indirectly
	 and hence must have all eliminations at their initial offsets.  */
      for (tem = REG_NOTES (x); tem; tem = XEXP (tem, 1))
	if (REG_NOTE_KIND (tem) == REG_LABEL)
	  set_label_offsets (XEXP (tem, 0), insn, 1);
      return;

    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      /* Each of the labels in the address vector must be at their initial
	 offsets.  We want the first first for ADDR_VEC and the second
	 field for ADDR_DIFF_VEC.  */

      for (i = 0; i < XVECLEN (x, code == ADDR_DIFF_VEC); i++)
	set_label_offsets (XVECEXP (x, code == ADDR_DIFF_VEC, i),
			   insn, initial_p);
      return;

    case SET:
      /* We only care about setting PC.  If the source is not RETURN,
	 IF_THEN_ELSE, or a label, disable any eliminations not at
	 their initial offsets.  Similarly if any arm of the IF_THEN_ELSE
	 isn't one of those possibilities.  For branches to a label,
	 call ourselves recursively.

	 Note that this can disable elimination unnecessarily when we have
	 a non-local goto since it will look like a non-constant jump to
	 someplace in the current function.  This isn't a significant
	 problem since such jumps will normally be when all elimination
	 pairs are back to their initial offsets.  */

      if (SET_DEST (x) != pc_rtx)
	return;

      switch (GET_CODE (SET_SRC (x)))
	{
	case PC:
	case RETURN:
	  return;

	case LABEL_REF:
	  set_label_offsets (XEXP (SET_SRC (x), 0), insn, initial_p);
	  return;

	case IF_THEN_ELSE:
	  tem = XEXP (SET_SRC (x), 1);
	  if (GET_CODE (tem) == LABEL_REF)
	    set_label_offsets (XEXP (tem, 0), insn, initial_p);
	  else if (GET_CODE (tem) != PC && GET_CODE (tem) != RETURN)
	    break;

	  tem = XEXP (SET_SRC (x), 2);
	  if (GET_CODE (tem) == LABEL_REF)
	    set_label_offsets (XEXP (tem, 0), insn, initial_p);
	  else if (GET_CODE (tem) != PC && GET_CODE (tem) != RETURN)
	    break;
	  return;
	}

      /* If we reach here, all eliminations must be at their initial
	 offset because we are doing a jump to a variable address.  */
      for (p = reg_eliminate; p < &reg_eliminate[NUM_ELIMINABLE_REGS]; p++)
	if (p->offset != p->initial_offset)
	  p->can_eliminate = 0;
    }
}

/* Used for communication between the next two function to properly share
   the vector for an ASM_OPERANDS.  */

static struct rtvec_def *old_asm_operands_vec, *new_asm_operands_vec;

/* Scan X and replace any eliminable registers (such as fp) with a
   replacement (such as sp), plus an offset.

   MEM_MODE is the mode of an enclosing MEM.  We need this to know how
   much to adjust a register for, e.g., PRE_DEC.  Also, if we are inside a
   MEM, we are allowed to replace a sum of a register and the constant zero
   with the register, which we cannot do outside a MEM.  In addition, we need
   to record the fact that a register is referenced outside a MEM.

   If INSN is nonzero, it is the insn containing X.  If we replace a REG
   in a SET_DEST with an equivalent MEM and INSN is non-zero, write a
   CLOBBER of the pseudo after INSN so find_equiv_regs will know that
   that the REG is being modified.

   If we see a modification to a register we know about, take the
   appropriate action (see case SET, below).

   REG_EQUIV_MEM and REG_EQUIV_ADDRESS contain address that have had
   replacements done assuming all offsets are at their initial values.  If
   they are not, or if REG_EQUIV_ADDRESS is nonzero for a pseudo we
   encounter, return the actual location so that find_reloads will do
   the proper thing.  */

rtx
eliminate_regs (x, mem_mode, insn)
     rtx x;
     enum machine_mode mem_mode;
     rtx insn;
{
  enum rtx_code code = GET_CODE (x);
  struct elim_table *ep;
  int regno;
  rtx new;
  int i, j;
  char *fmt;
  int copied = 0;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case RETURN:
      return x;

    case REG:
      regno = REGNO (x);

      /* First handle the case where we encounter a bare register that
	 is eliminable.  Replace it with a PLUS.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->from_rtx == x && ep->can_eliminate)
	      {
		if (! mem_mode)
		  ep->ref_outside_mem = 1;
		return plus_constant (ep->to_rtx, ep->previous_offset);
	      }

	}
      else if (reg_equiv_memory_loc && reg_equiv_memory_loc[regno]
	       && (reg_equiv_address[regno] || num_not_at_initial_offset))
	{
	  /* In this case, find_reloads would attempt to either use an
	     incorrect address (if something is not at its initial offset)
	     or substitute an replaced address into an insn (which loses
	     if the offset is changed by some later action).  So we simply
	     return the replaced stack slot (assuming it is changed by
	     elimination) and ignore the fact that this is actually a
	     reference to the pseudo.  Ensure we make a copy of the
	     address in case it is shared.  */
	  new = eliminate_regs (reg_equiv_memory_loc[regno],
				mem_mode, NULL_RTX);
	  if (new != reg_equiv_memory_loc[regno])
	    return copy_rtx (new);
	}
      return x;

    case PLUS:
      /* If this is the sum of an eliminable register and a constant, rework
	 the sum.   */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
	  && CONSTANT_P (XEXP (x, 1)))
	{
	  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->from_rtx == XEXP (x, 0) && ep->can_eliminate)
	      {
		if (! mem_mode)
		  ep->ref_outside_mem = 1;

		/* The only time we want to replace a PLUS with a REG (this
		   occurs when the constant operand of the PLUS is the negative
		   of the offset) is when we are inside a MEM.  We won't want
		   to do so at other times because that would change the
		   structure of the insn in a way that reload can't handle.
		   We special-case the commonest situation in
		   eliminate_regs_in_insn, so just replace a PLUS with a
		   PLUS here, unless inside a MEM.  */
		if (mem_mode != 0 && GET_CODE (XEXP (x, 1)) == CONST_INT
		    && INTVAL (XEXP (x, 1)) == - ep->previous_offset)
		  return ep->to_rtx;
		else
		  return gen_rtx (PLUS, Pmode, ep->to_rtx,
				  plus_constant (XEXP (x, 1),
						 ep->previous_offset));
	      }

	  /* If the register is not eliminable, we are done since the other
	     operand is a constant.  */
	  return x;
	}

      /* If this is part of an address, we want to bring any constant to the
	 outermost PLUS.  We will do this by doing register replacement in
	 our operands and seeing if a constant shows up in one of them.

	 We assume here this is part of an address (or a "load address" insn)
	 since an eliminable register is not likely to appear in any other
	 context.

	 If we have (plus (eliminable) (reg)), we want to produce
	 (plus (plus (replacement) (reg) (const))).  If this was part of a
	 normal add insn, (plus (replacement) (reg)) will be pushed as a
	 reload.  This is the desired action.  */

      {
	rtx new0 = eliminate_regs (XEXP (x, 0), mem_mode, NULL_RTX);
	rtx new1 = eliminate_regs (XEXP (x, 1), mem_mode, NULL_RTX);

	if (new0 != XEXP (x, 0) || new1 != XEXP (x, 1))
	  {
	    /* If one side is a PLUS and the other side is a pseudo that
	       didn't get a hard register but has a reg_equiv_constant,
	       we must replace the constant here since it may no longer
	       be in the position of any operand.  */
	    if (GET_CODE (new0) == PLUS && GET_CODE (new1) == REG
		&& REGNO (new1) >= FIRST_PSEUDO_REGISTER
		&& reg_renumber[REGNO (new1)] < 0
		&& reg_equiv_constant != 0
		&& reg_equiv_constant[REGNO (new1)] != 0)
	      new1 = reg_equiv_constant[REGNO (new1)];
	    else if (GET_CODE (new1) == PLUS && GET_CODE (new0) == REG
		     && REGNO (new0) >= FIRST_PSEUDO_REGISTER
		     && reg_renumber[REGNO (new0)] < 0
		     && reg_equiv_constant[REGNO (new0)] != 0)
	      new0 = reg_equiv_constant[REGNO (new0)];

	    new = form_sum (new0, new1);

	    /* As above, if we are not inside a MEM we do not want to
	       turn a PLUS into something else.  We might try to do so here
	       for an addition of 0 if we aren't optimizing.  */
	    if (! mem_mode && GET_CODE (new) != PLUS)
	      return gen_rtx (PLUS, GET_MODE (x), new, const0_rtx);
	    else
	      return new;
	  }
      }
      return x;

    case EXPR_LIST:
      /* If we have something in XEXP (x, 0), the usual case, eliminate it.  */
      if (XEXP (x, 0))
	{
	  new = eliminate_regs (XEXP (x, 0), mem_mode, NULL_RTX);
	  if (new != XEXP (x, 0))
	    x = gen_rtx (EXPR_LIST, REG_NOTE_KIND (x), new, XEXP (x, 1));
	}

      /* ... fall through ... */

    case INSN_LIST:
      /* Now do eliminations in the rest of the chain.  If this was
	 an EXPR_LIST, this might result in allocating more memory than is
	 strictly needed, but it simplifies the code.  */
      if (XEXP (x, 1))
	{
	  new = eliminate_regs (XEXP (x, 1), mem_mode, NULL_RTX);
	  if (new != XEXP (x, 1))
	    return gen_rtx (INSN_LIST, GET_MODE (x), XEXP (x, 0), new);
	}
      return x;

    case CALL:
    case COMPARE:
    case MINUS:
    case MULT:
    case DIV:      case UDIV:
    case MOD:      case UMOD:
    case AND:      case IOR:      case XOR:
    case LSHIFT:   case ASHIFT:   case ROTATE:
    case ASHIFTRT: case LSHIFTRT: case ROTATERT:
    case NE:       case EQ:
    case GE:       case GT:       case GEU:    case GTU:
    case LE:       case LT:       case LEU:    case LTU:
      {
	rtx new0 = eliminate_regs (XEXP (x, 0), mem_mode, NULL_RTX);
	rtx new1
	  = XEXP (x, 1) ? eliminate_regs (XEXP (x, 1), mem_mode, NULL_RTX) : 0;

	if (new0 != XEXP (x, 0) || new1 != XEXP (x, 1))
	  return gen_rtx (code, GET_MODE (x), new0, new1);
      }
      return x;

    case PRE_INC:
    case POST_INC:
    case PRE_DEC:
    case POST_DEC:
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if (ep->to_rtx == XEXP (x, 0))
	  {
	    if (code == PRE_DEC || code == POST_DEC)
	      ep->offset += GET_MODE_SIZE (mem_mode);
	    else
	      ep->offset -= GET_MODE_SIZE (mem_mode);
	  }

      /* Fall through to generic unary operation case.  */
    case USE:
    case STRICT_LOW_PART:
    case NEG:          case NOT:
    case SIGN_EXTEND:  case ZERO_EXTEND:
    case TRUNCATE:     case FLOAT_EXTEND: case FLOAT_TRUNCATE:
    case FLOAT:        case FIX:
    case UNSIGNED_FIX: case UNSIGNED_FLOAT:
    case ABS:
    case SQRT:
    case FFS:
      new = eliminate_regs (XEXP (x, 0), mem_mode, NULL_RTX);
      if (new != XEXP (x, 0))
	return gen_rtx (code, GET_MODE (x), new);
      return x;

    case SUBREG:
      /* Similar to above processing, but preserve SUBREG_WORD.
	 Convert (subreg (mem)) to (mem) if not paradoxical.
	 Also, if we have a non-paradoxical (subreg (pseudo)) and the
	 pseudo didn't get a hard reg, we must replace this with the
	 eliminated version of the memory location because push_reloads
	 may do the replacement in certain circumstances.  */
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && (GET_MODE_SIZE (GET_MODE (x))
	      <= GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	  && reg_equiv_memory_loc != 0
	  && reg_equiv_memory_loc[REGNO (SUBREG_REG (x))] != 0)
	{
	  new = eliminate_regs (reg_equiv_memory_loc[REGNO (SUBREG_REG (x))],
				mem_mode, NULL_RTX);

	  /* If we didn't change anything, we must retain the pseudo.  */
	  if (new == reg_equiv_memory_loc[REGNO (SUBREG_REG (x))])
	    new = XEXP (x, 0);
	  else
	    /* Otherwise, ensure NEW isn't shared in case we have to reload
	       it.  */
	    new = copy_rtx (new);
	}
      else
	new = eliminate_regs (SUBREG_REG (x), mem_mode, NULL_RTX);

      if (new != XEXP (x, 0))
	{
	  if (GET_CODE (new) == MEM
	      && (GET_MODE_SIZE (GET_MODE (x))
		  <= GET_MODE_SIZE (GET_MODE (new))))
	    {
	      int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
	      enum machine_mode mode = GET_MODE (x);

#if BYTES_BIG_ENDIAN
	      offset += (MIN (UNITS_PER_WORD,
			      GET_MODE_SIZE (GET_MODE (new)))
			 - MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode)));
#endif

	      PUT_MODE (new, mode);
	      XEXP (new, 0) = plus_constant (XEXP (new, 0), offset);
	      return new;
	    }
	  else
	    return gen_rtx (SUBREG, GET_MODE (x), new, SUBREG_WORD (x));
	}

      return x;

    case CLOBBER:
      /* If clobbering a register that is the replacement register for an
	 elimination we still think can be performed, note that it cannot
	 be performed.  Otherwise, we need not be concerned about it.  */
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if (ep->to_rtx == XEXP (x, 0))
	  ep->can_eliminate = 0;

      return x;

    case ASM_OPERANDS:
      {
	rtx *temp_vec;
	/* Properly handle sharing input and constraint vectors.  */
	if (ASM_OPERANDS_INPUT_VEC (x) != old_asm_operands_vec)
	  {
	    /* When we come to a new vector not seen before,
	       scan all its elements; keep the old vector if none
	       of them changes; otherwise, make a copy.  */
	    old_asm_operands_vec = ASM_OPERANDS_INPUT_VEC (x);
	    temp_vec = (rtx *) alloca (XVECLEN (x, 3) * sizeof (rtx));
	    for (i = 0; i < ASM_OPERANDS_INPUT_LENGTH (x); i++)
	      temp_vec[i] = eliminate_regs (ASM_OPERANDS_INPUT (x, i),
					    mem_mode, NULL_RTX);

	    for (i = 0; i < ASM_OPERANDS_INPUT_LENGTH (x); i++)
	      if (temp_vec[i] != ASM_OPERANDS_INPUT (x, i))
		break;

	    if (i == ASM_OPERANDS_INPUT_LENGTH (x))
	      new_asm_operands_vec = old_asm_operands_vec;
	    else
	      new_asm_operands_vec
		= gen_rtvec_v (ASM_OPERANDS_INPUT_LENGTH (x), temp_vec);
	  }

	/* If we had to copy the vector, copy the entire ASM_OPERANDS.  */
	if (new_asm_operands_vec == old_asm_operands_vec)
	  return x;

	new = gen_rtx (ASM_OPERANDS, VOIDmode, ASM_OPERANDS_TEMPLATE (x),
		       ASM_OPERANDS_OUTPUT_CONSTRAINT (x),
		       ASM_OPERANDS_OUTPUT_IDX (x), new_asm_operands_vec,
		       ASM_OPERANDS_INPUT_CONSTRAINT_VEC (x),
		       ASM_OPERANDS_SOURCE_FILE (x),
		       ASM_OPERANDS_SOURCE_LINE (x));
	new->volatil = x->volatil;
	return new;
      }

    case SET:
      /* Check for setting a register that we know about.  */
      if (GET_CODE (SET_DEST (x)) == REG)
	{
	  /* See if this is setting the replacement register for an
	     elimination.

	     If DEST is the frame pointer, we do nothing because we assume that
	     all assignments to the frame pointer are for non-local gotos and
	     are being done at a time when they are valid and do not disturb
	     anything else.  Some machines want to eliminate a fake argument
	     pointer with either the frame or stack pointer.  Assignments to
	     the frame pointer must not prevent this elimination.  */

	  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->to_rtx == SET_DEST (x)
		&& SET_DEST (x) != frame_pointer_rtx)
	      {
		/* If it is being incremented, adjust the offset.  Otherwise,
		   this elimination can't be done.  */
		rtx src = SET_SRC (x);

		if (GET_CODE (src) == PLUS
		    && XEXP (src, 0) == SET_DEST (x)
		    && GET_CODE (XEXP (src, 1)) == CONST_INT)
		  ep->offset -= INTVAL (XEXP (src, 1));
		else
		  ep->can_eliminate = 0;
	      }

	  /* Now check to see we are assigning to a register that can be
	     eliminated.  If so, it must be as part of a PARALLEL, since we
	     will not have been called if this is a single SET.  So indicate
	     that we can no longer eliminate this reg.  */
	  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	       ep++)
	    if (ep->from_rtx == SET_DEST (x) && ep->can_eliminate)
	      ep->can_eliminate = 0;
	}

      /* Now avoid the loop below in this common case.  */
      {
	rtx new0 = eliminate_regs (SET_DEST (x), 0, NULL_RTX);
	rtx new1 = eliminate_regs (SET_SRC (x), 0, NULL_RTX);

	/* If SET_DEST changed from a REG to a MEM and INSN is non-zero,
	   write a CLOBBER insn.  */
	if (GET_CODE (SET_DEST (x)) == REG && GET_CODE (new0) == MEM
	    && insn != 0)
	  emit_insn_after (gen_rtx (CLOBBER, VOIDmode, SET_DEST (x)), insn);

	if (new0 != SET_DEST (x) || new1 != SET_SRC (x))
	  return gen_rtx (SET, VOIDmode, new0, new1);
      }

      return x;

    case MEM:
      /* Our only special processing is to pass the mode of the MEM to our
	 recursive call and copy the flags.  While we are here, handle this
	 case more efficiently.  */
      new = eliminate_regs (XEXP (x, 0), GET_MODE (x), NULL_RTX);
      if (new != XEXP (x, 0))
	{
	  new = gen_rtx (MEM, GET_MODE (x), new);
	  new->volatil = x->volatil;
	  new->unchanging = x->unchanging;
	  new->in_struct = x->in_struct;
	  return new;
	}
      else
	return x;
    }

  /* Process each of our operands recursively.  If any have changed, make a
     copy of the rtx.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++, fmt++)
    {
      if (*fmt == 'e')
	{
	  new = eliminate_regs (XEXP (x, i), mem_mode, NULL_RTX);
	  if (new != XEXP (x, i) && ! copied)
	    {
	      rtx new_x = rtx_alloc (code);
	      bcopy (x, new_x, (sizeof (*new_x) - sizeof (new_x->fld)
				+ (sizeof (new_x->fld[0])
				   * GET_RTX_LENGTH (code))));
	      x = new_x;
	      copied = 1;
	    }
	  XEXP (x, i) = new;
	}
      else if (*fmt == 'E')
	{
	  int copied_vec = 0;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      new = eliminate_regs (XVECEXP (x, i, j), mem_mode, insn);
	      if (new != XVECEXP (x, i, j) && ! copied_vec)
		{
		  rtvec new_v = gen_rtvec_v (XVECLEN (x, i),
					     &XVECEXP (x, i, 0));
		  if (! copied)
		    {
		      rtx new_x = rtx_alloc (code);
		      bcopy (x, new_x, (sizeof (*new_x) - sizeof (new_x->fld)
					+ (sizeof (new_x->fld[0])
					   * GET_RTX_LENGTH (code))));
		      x = new_x;
		      copied = 1;
		    }
		  XVEC (x, i) = new_v;
		  copied_vec = 1;
		}
	      XVECEXP (x, i, j) = new;
	    }
	}
    }

  return x;
}

/* Scan INSN and eliminate all eliminable registers in it.

   If REPLACE is nonzero, do the replacement destructively.  Also
   delete the insn as dead it if it is setting an eliminable register.

   If REPLACE is zero, do all our allocations in reload_obstack.

   If no eliminations were done and this insn doesn't require any elimination
   processing (these are not identical conditions: it might be updating sp,
   but not referencing fp; this needs to be seen during reload_as_needed so
   that the offset between fp and sp can be taken into consideration), zero
   is returned.  Otherwise, 1 is returned.  */

static int
eliminate_regs_in_insn (insn, replace)
     rtx insn;
     int replace;
{
  rtx old_body = PATTERN (insn);
  rtx new_body;
  int val = 0;
  struct elim_table *ep;

  if (! replace)
    push_obstacks (&reload_obstack, &reload_obstack);

  if (GET_CODE (old_body) == SET && GET_CODE (SET_DEST (old_body)) == REG
      && REGNO (SET_DEST (old_body)) < FIRST_PSEUDO_REGISTER)
    {
      /* Check for setting an eliminable register.  */
      for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
	if (ep->from_rtx == SET_DEST (old_body) && ep->can_eliminate)
	  {
	    /* In this case this insn isn't serving a useful purpose.  We
	       will delete it in reload_as_needed once we know that this
	       elimination is, in fact, being done.

	       If REPLACE isn't set, we can't delete this insn, but neededn't
	       process it since it won't be used unless something changes.  */
	    if (replace)
	      delete_dead_insn (insn);
	    val = 1;
	    goto done;
	  }

      /* Check for (set (reg) (plus (reg from) (offset))) where the offset
	 in the insn is the negative of the offset in FROM.  Substitute
	 (set (reg) (reg to)) for the insn and change its code.

	 We have to do this here, rather than in eliminate_regs, do that we can
	 change the insn code.  */

      if (GET_CODE (SET_SRC (old_body)) == PLUS
	  && GET_CODE (XEXP (SET_SRC (old_body), 0)) == REG
	  && GET_CODE (XEXP (SET_SRC (old_body), 1)) == CONST_INT)
	for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS];
	     ep++)
	  if (ep->from_rtx == XEXP (SET_SRC (old_body), 0)
	      && ep->can_eliminate
	      && ep->offset == - INTVAL (XEXP (SET_SRC (old_body), 1)))
	    {
	      PATTERN (insn) = gen_rtx (SET, VOIDmode,
					SET_DEST (old_body), ep->to_rtx);
	      INSN_CODE (insn) = -1;
	      val = 1;
	      goto done;
	    }
    }

  old_asm_operands_vec = 0;

  /* Replace the body of this insn with a substituted form.  If we changed
     something, return non-zero.  If this is the final call for this
     insn (REPLACE is non-zero), do the elimination in REG_NOTES as well.

     If we are replacing a body that was a (set X (plus Y Z)), try to
     re-recognize the insn.  We do this in case we had a simple addition
     but now can do this as a load-address.  This saves an insn in this
     common case. */

  new_body = eliminate_regs (old_body, 0, replace ? insn : NULL_RTX);
  if (new_body != old_body)
    {
      /* If we aren't replacing things permanently and we changed something,
	 make another copy to ensure that all the RTL is new.  Otherwise
	 things can go wrong if find_reload swaps commutative operands
	 and one is inside RTL that has been copied while the other is not. */

      /* Don't copy an asm_operands because (1) there's no need and (2)
	 copy_rtx can't do it properly when there are multiple outputs.  */
      if (! replace && asm_noperands (old_body) < 0)
	new_body = copy_rtx (new_body);

      /* If we had a move insn but now we don't, rerecognize it.  */
      if ((GET_CODE (old_body) == SET && GET_CODE (SET_SRC (old_body)) == REG
	   && (GET_CODE (new_body) != SET
	       || GET_CODE (SET_SRC (new_body)) != REG))
	  /* If this was an add insn before, rerecognize.  */
	  ||
	  (GET_CODE (old_body) == SET
	   && GET_CODE (SET_SRC (old_body)) == PLUS))
	{
	  if (! validate_change (insn, &PATTERN (insn), new_body, 0))
	    /* If recognition fails, store the new body anyway.
	       It's normal to have recognition failures here
	       due to bizarre memory addresses; reloading will fix them.  */
	    PATTERN (insn) = new_body;
	}
      else
	PATTERN (insn) = new_body;

      if (replace && REG_NOTES (insn))
	REG_NOTES (insn) = eliminate_regs (REG_NOTES (insn), 0, NULL_RTX);
      val = 1;
    }

  /* Loop through all elimination pairs.  See if any have changed and
     recalculate the number not at initial offset.

     Compute the maximum offset (minimum offset if the stack does not
     grow downward) for each elimination pair.

     We also detect a cases where register elimination cannot be done,
     namely, if a register would be both changed and referenced outside a MEM
     in the resulting insn since such an insn is often undefined and, even if
     not, we cannot know what meaning will be given to it.  Note that it is
     valid to have a register used in an address in an insn that changes it
     (presumably with a pre- or post-increment or decrement).

     If anything changes, return nonzero.  */

  num_not_at_initial_offset = 0;
  for (ep = reg_eliminate; ep < &reg_eliminate[NUM_ELIMINABLE_REGS]; ep++)
    {
      if (ep->previous_offset != ep->offset && ep->ref_outside_mem)
	ep->can_eliminate = 0;

      ep->ref_outside_mem = 0;

      if (ep->previous_offset != ep->offset)
	val = 1;

      ep->previous_offset = ep->offset;
      if (ep->can_eliminate && ep->offset != ep->initial_offset)
	num_not_at_initial_offset++;

#ifdef STACK_GROWS_DOWNWARD
      ep->max_offset = MAX (ep->max_offset, ep->offset);
#else
      ep->max_offset = MIN (ep->max_offset, ep->offset);
#endif
    }

 done:
  if (! replace)
    pop_obstacks ();

  return val;
}

/* Given X, a SET or CLOBBER of DEST, if DEST is the target of a register
   replacement we currently believe is valid, mark it as not eliminable if X
   modifies DEST in any way other than by adding a constant integer to it.

   If DEST is the frame pointer, we do nothing because we assume that
   all assignments to the frame pointer are nonlocal gotos and are being done
   at a time when they are valid and do not disturb anything else.
   Some machines want to eliminate a fake argument pointer with either the
   frame or stack pointer.  Assignments to the frame pointer must not prevent
   this elimination.

   Called via note_stores from reload before starting its passes to scan
   the insns of the function.  */

static void
mark_not_eliminable (dest, x)
     rtx dest;
     rtx x;
{
  register int i;

  /* A SUBREG of a hard register here is just changing its mode.  We should
     not see a SUBREG of an eliminable hard register, but check just in
     case.  */
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (dest == frame_pointer_rtx)
    return;

  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
    if (reg_eliminate[i].can_eliminate && dest == reg_eliminate[i].to_rtx
	&& (GET_CODE (x) != SET
	    || GET_CODE (SET_SRC (x)) != PLUS
	    || XEXP (SET_SRC (x), 0) != dest
	    || GET_CODE (XEXP (SET_SRC (x), 1)) != CONST_INT))
      {
	reg_eliminate[i].can_eliminate_previous
	  = reg_eliminate[i].can_eliminate = 0;
	num_eliminable--;
      }
}

/* Kick all pseudos out of hard register REGNO.
   If GLOBAL is nonzero, try to find someplace else to put them.
   If DUMPFILE is nonzero, log actions taken on that file.

   If CANT_ELIMINATE is nonzero, it means that we are doing this spill
   because we found we can't eliminate some register.  In the case, no pseudos
   are allowed to be in the register, even if they are only in a block that
   doesn't require spill registers, unlike the case when we are spilling this
   hard reg to produce another spill register.

   Return nonzero if any pseudos needed to be kicked out.  */

static int
spill_hard_reg (regno, global, dumpfile, cant_eliminate)
     register int regno;
     int global;
     FILE *dumpfile;
     int cant_eliminate;
{
  int something_changed = 0;
  register int i;

  SET_HARD_REG_BIT (forbidden_regs, regno);

  /* Spill every pseudo reg that was allocated to this reg
     or to something that overlaps this reg.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] >= 0
	&& reg_renumber[i] <= regno
	&& (reg_renumber[i]
	    + HARD_REGNO_NREGS (reg_renumber[i],
				PSEUDO_REGNO_MODE (i))
	    > regno))
      {
	enum reg_class class = REGNO_REG_CLASS (regno);

	/* If this register belongs solely to a basic block which needed no
	   spilling of any class that this register is contained in,
	   leave it be, unless we are spilling this register because
	   it was a hard register that can't be eliminated.   */

	if (! cant_eliminate
	    && basic_block_needs[0]
	    && reg_basic_block[i] >= 0
	    && basic_block_needs[(int) class][reg_basic_block[i]] == 0)
	  {
	    enum reg_class *p;

	    for (p = reg_class_superclasses[(int) class];
		 *p != LIM_REG_CLASSES; p++)
	      if (basic_block_needs[(int) *p][reg_basic_block[i]] > 0)
		break;

	    if (*p == LIM_REG_CLASSES)
	      continue;
	  }

	/* Mark it as no longer having a hard register home.  */
	reg_renumber[i] = -1;
	/* We will need to scan everything again.  */
	something_changed = 1;
	if (global)
	    retry_global_alloc (i, forbidden_regs);

	alter_reg (i, regno);
	if (dumpfile)
	  {
	    if (reg_renumber[i] == -1)
	      fprintf (dumpfile, " Register %d now on stack.\n\n", i);
	    else
	      fprintf (dumpfile, " Register %d now in %d.\n\n",
		       i, reg_renumber[i]);
	  }
      }

  return something_changed;
}

/* Find all paradoxical subregs within X and update reg_max_ref_width.  */

static void
scan_paradoxical_subregs (x)
     register rtx x;
{
  register int i;
  register char *fmt;
  register enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case REG:
    case USE:
    case CLOBBER:
      return;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && GET_MODE_SIZE (GET_MODE (x)) > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	reg_max_ref_width[REGNO (SUBREG_REG (x))]
	  = GET_MODE_SIZE (GET_MODE (x));
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_paradoxical_subregs (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    scan_paradoxical_subregs (XVECEXP (x, i, j));
	}
    }
}

struct hard_reg_n_uses { int regno; int uses; };

static int
hard_reg_use_compare (p1, p2)
     struct hard_reg_n_uses *p1, *p2;
{
  int tem = p1->uses - p2->uses;
  if (tem != 0) return tem;
  /* If regs are equally good, sort by regno,
     so that the results of qsort leave nothing to chance.  */
  return p1->regno - p2->regno;
}

/* Choose the order to consider regs for use as reload registers
   based on how much trouble would be caused by spilling one.
   Store them in order of decreasing preference in potential_reload_regs.  */

static void
order_regs_for_reload ()
{
  register int i;
  register int o = 0;
  int large = 0;

  struct hard_reg_n_uses hard_reg_n_uses[FIRST_PSEUDO_REGISTER];

  CLEAR_HARD_REG_SET (bad_spill_regs);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    potential_reload_regs[i] = -1;

  /* Count number of uses of each hard reg by pseudo regs allocated to it
     and then order them by decreasing use.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      hard_reg_n_uses[i].uses = 0;
      hard_reg_n_uses[i].regno = i;
    }

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    {
      int regno = reg_renumber[i];
      if (regno >= 0)
	{
	  int lim = regno + HARD_REGNO_NREGS (regno, PSEUDO_REGNO_MODE (i));
	  while (regno < lim)
	    hard_reg_n_uses[regno++].uses += reg_n_refs[i];
	}
      large += reg_n_refs[i];
    }

  /* Now fixed registers (which cannot safely be used for reloading)
     get a very high use count so they will be considered least desirable.
     Registers used explicitly in the rtl code are almost as bad.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (fixed_regs[i])
	{
	  hard_reg_n_uses[i].uses += 2 * large + 2;
	  SET_HARD_REG_BIT (bad_spill_regs, i);
	}
      else if (regs_explicitly_used[i])
	{
	  hard_reg_n_uses[i].uses += large + 1;
	  /* ??? We are doing this here because of the potential that
	     bad code may be generated if a register explicitly used in
	     an insn was used as a spill register for that insn.  But
	     not using these are spill registers may lose on some machine.
	     We'll have to see how this works out.  */
	  SET_HARD_REG_BIT (bad_spill_regs, i);
	}
    }
  hard_reg_n_uses[FRAME_POINTER_REGNUM].uses += 2 * large + 2;
  SET_HARD_REG_BIT (bad_spill_regs, FRAME_POINTER_REGNUM);

#ifdef ELIMINABLE_REGS
  /* If registers other than the frame pointer are eliminable, mark them as
     poor choices.  */
  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
    {
      hard_reg_n_uses[reg_eliminate[i].from].uses += 2 * large + 2;
      SET_HARD_REG_BIT (bad_spill_regs, reg_eliminate[i].from);
    }
#endif

  /* Prefer registers not so far used, for use in temporary loading.
     Among them, if REG_ALLOC_ORDER is defined, use that order.
     Otherwise, prefer registers not preserved by calls.  */

#ifdef REG_ALLOC_ORDER
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int regno = reg_alloc_order[i];

      if (hard_reg_n_uses[regno].uses == 0)
	potential_reload_regs[o++] = regno;
    }
#else
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (hard_reg_n_uses[i].uses == 0 && call_used_regs[i])
	potential_reload_regs[o++] = i;
    }
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (hard_reg_n_uses[i].uses == 0 && ! call_used_regs[i])
	potential_reload_regs[o++] = i;
    }
#endif

  qsort (hard_reg_n_uses, FIRST_PSEUDO_REGISTER,
	 sizeof hard_reg_n_uses[0], hard_reg_use_compare);

  /* Now add the regs that are already used,
     preferring those used less often.  The fixed and otherwise forbidden
     registers will be at the end of this list.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (hard_reg_n_uses[i].uses != 0)
      potential_reload_regs[o++] = hard_reg_n_uses[i].regno;
}

/* Reload pseudo-registers into hard regs around each insn as needed.
   Additional register load insns are output before the insn that needs it
   and perhaps store insns after insns that modify the reloaded pseudo reg.

   reg_last_reload_reg and reg_reloaded_contents keep track of
   which pseudo-registers are already available in reload registers.
   We update these for the reloads that we perform,
   as the insns are scanned.  */

static void
reload_as_needed (first, live_known)
     rtx first;
     int live_known;
{
  register rtx insn;
  register int i;
  int this_block = 0;
  rtx x;
  rtx after_call = 0;

  bzero (spill_reg_rtx, sizeof spill_reg_rtx);
  reg_last_reload_reg = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_last_reload_reg, max_regno * sizeof (rtx));
  reg_has_output_reload = (char *) alloca (max_regno);
  for (i = 0; i < n_spills; i++)
    {
      reg_reloaded_contents[i] = -1;
      reg_reloaded_insn[i] = 0;
    }

  /* Reset all offsets on eliminable registers to their initial values.  */
#ifdef ELIMINABLE_REGS
  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
    {
      INITIAL_ELIMINATION_OFFSET (reg_eliminate[i].from, reg_eliminate[i].to,
				  reg_eliminate[i].initial_offset);
      reg_eliminate[i].previous_offset
	= reg_eliminate[i].offset = reg_eliminate[i].initial_offset;
    }
#else
  INITIAL_FRAME_POINTER_OFFSET (reg_eliminate[0].initial_offset);
  reg_eliminate[0].previous_offset
    = reg_eliminate[0].offset = reg_eliminate[0].initial_offset;
#endif

  num_not_at_initial_offset = 0;

  for (insn = first; insn;)
    {
      register rtx next = NEXT_INSN (insn);

      /* Notice when we move to a new basic block.  */
      if (live_known && this_block + 1 < n_basic_blocks
	  && insn == basic_block_head[this_block+1])
	++this_block;

      /* If we pass a label, copy the offsets from the label information
	 into the current offsets of each elimination.  */
      if (GET_CODE (insn) == CODE_LABEL)
	{
	  num_not_at_initial_offset = 0;
	  for (i = 0; i < NUM_ELIMINABLE_REGS; i++)
	    {
	      reg_eliminate[i].offset = reg_eliminate[i].previous_offset
		= offsets_at[CODE_LABEL_NUMBER (insn)][i];
	      if (reg_eliminate[i].can_eliminate
		  && (reg_eliminate[i].offset
		      != reg_eliminate[i].initial_offset))
		num_not_at_initial_offset++;
	    }
	}

      else if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  rtx avoid_return_reg = 0;

#ifdef SMALL_REGISTER_CLASSES
	  /* Set avoid_return_reg if this is an insn
	     that might use the value of a function call.  */
	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      if (GET_CODE (PATTERN (insn)) == SET)
		after_call = SET_DEST (PATTERN (insn));
	      else if (GET_CODE (PATTERN (insn)) == PARALLEL
		       && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
		after_call = SET_DEST (XVECEXP (PATTERN (insn), 0, 0));
	      else
		after_call = 0;
	    }
	  else if (after_call != 0
		   && !(GET_CODE (PATTERN (insn)) == SET
			&& SET_DEST (PATTERN (insn)) == stack_pointer_rtx))
	    {
	      if (reg_mentioned_p (after_call, PATTERN (insn)))
		avoid_return_reg = after_call;
	      after_call = 0;
	    }
#endif /* SMALL_REGISTER_CLASSES */

	  /* If this is a USE and CLOBBER of a MEM, ensure that any
	     references to eliminable registers have been removed.  */

	  if ((GET_CODE (PATTERN (insn)) == USE
	       || GET_CODE (PATTERN (insn)) == CLOBBER)
	      && GET_CODE (XEXP (PATTERN (insn), 0)) == MEM)
	    XEXP (XEXP (PATTERN (insn), 0), 0)
	      = eliminate_regs (XEXP (XEXP (PATTERN (insn), 0), 0),
				GET_MODE (XEXP (PATTERN (insn), 0)), NULL_RTX);

	  /* If we need to do register elimination processing, do so.
	     This might delete the insn, in which case we are done.  */
	  if (num_eliminable && GET_MODE (insn) == QImode)
	    {
	      eliminate_regs_in_insn (insn, 1);
	      if (GET_CODE (insn) == NOTE)
		{
		  insn = next;
		  continue;
		}
	    }

	  if (GET_MODE (insn) == VOIDmode)
	    n_reloads = 0;
	  /* First find the pseudo regs that must be reloaded for this insn.
	     This info is returned in the tables reload_... (see reload.h).
	     Also modify the body of INSN by substituting RELOAD
	     rtx's for those pseudo regs.  */
	  else
	    {
	      bzero (reg_has_output_reload, max_regno);
	      CLEAR_HARD_REG_SET (reg_is_output_reload);

	      find_reloads (insn, 1, spill_indirect_levels, live_known,
			    spill_reg_order);
	    }

	  if (n_reloads > 0)
	    {
	      rtx prev = PREV_INSN (insn), next = NEXT_INSN (insn);
	      rtx p;
	      int class;

	      /* If this block has not had spilling done for a
		 particular class, deactivate any optional reloads
		 of that class lest they try to use a spill-reg which isn't
		 available here.  If we have any non-optionals that need a
		 spill reg, abort.  */

	      for (class = 0; class < N_REG_CLASSES; class++)
		if (basic_block_needs[class] != 0
		    && basic_block_needs[class][this_block] == 0)
		  for (i = 0; i < n_reloads; i++)
		    if (class == (int) reload_reg_class[i])
		      {
			if (reload_optional[i])
			  {
			    reload_in[i] = reload_out[i] = 0;
			    reload_secondary_p[i] = 0;
			  }
			else if (reload_reg_rtx[i] == 0
				 && (reload_in[i] != 0 || reload_out[i] != 0
				     || reload_secondary_p[i] != 0))
			  abort ();
		      }

	      /* Now compute which reload regs to reload them into.  Perhaps
		 reusing reload regs from previous insns, or else output
		 load insns to reload them.  Maybe output store insns too.
		 Record the choices of reload reg in reload_reg_rtx.  */
	      choose_reload_regs (insn, avoid_return_reg);

	      /* Generate the insns to reload operands into or out of
		 their reload regs.  */
	      emit_reload_insns (insn);

	      /* Substitute the chosen reload regs from reload_reg_rtx
		 into the insn's body (or perhaps into the bodies of other
		 load and store insn that we just made for reloading
		 and that we moved the structure into).  */
	      subst_reloads ();

	      /* If this was an ASM, make sure that all the reload insns
		 we have generated are valid.  If not, give an error
		 and delete them.  */

	      if (asm_noperands (PATTERN (insn)) >= 0)
		for (p = NEXT_INSN (prev); p != next; p = NEXT_INSN (p))
		  if (p != insn && GET_RTX_CLASS (GET_CODE (p)) == 'i'
		      && (recog_memoized (p) < 0
			  || (insn_extract (p),
			      ! constrain_operands (INSN_CODE (p), 1))))
		    {
		      error_for_asm (insn,
				     "`asm' operand requires impossible reload");
		      PUT_CODE (p, NOTE);
		      NOTE_SOURCE_FILE (p) = 0;
		      NOTE_LINE_NUMBER (p) = NOTE_INSN_DELETED;
		    }
	    }
	  /* Any previously reloaded spilled pseudo reg, stored in this insn,
	     is no longer validly lying around to save a future reload.
	     Note that this does not detect pseudos that were reloaded
	     for this insn in order to be stored in
	     (obeying register constraints).  That is correct; such reload
	     registers ARE still valid.  */
	  note_stores (PATTERN (insn), forget_old_reloads_1);

	  /* There may have been CLOBBER insns placed after INSN.  So scan
	     between INSN and NEXT and use them to forget old reloads.  */
	  for (x = NEXT_INSN (insn); x != next; x = NEXT_INSN (x))
	    if (GET_CODE (x) == INSN && GET_CODE (PATTERN (x)) == CLOBBER)
	      note_stores (PATTERN (x), forget_old_reloads_1);

#ifdef AUTO_INC_DEC
	  /* Likewise for regs altered by auto-increment in this insn.
	     But note that the reg-notes are not changed by reloading:
	     they still contain the pseudo-regs, not the spill regs.  */
	  for (x = REG_NOTES (insn); x; x = XEXP (x, 1))
	    if (REG_NOTE_KIND (x) == REG_INC)
	      {
		/* See if this pseudo reg was reloaded in this insn.
		   If so, its last-reload info is still valid
		   because it is based on this insn's reload.  */
		for (i = 0; i < n_reloads; i++)
		  if (reload_out[i] == XEXP (x, 0))
		    break;

		if (i != n_reloads)
		  forget_old_reloads_1 (XEXP (x, 0));
	      }
#endif
	}
      /* A reload reg's contents are unknown after a label.  */
      if (GET_CODE (insn) == CODE_LABEL)
	for (i = 0; i < n_spills; i++)
	  {
	    reg_reloaded_contents[i] = -1;
	    reg_reloaded_insn[i] = 0;
	  }

      /* Don't assume a reload reg is still good after a call insn
	 if it is a call-used reg.  */
      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == CALL_INSN)
	for (i = 0; i < n_spills; i++)
	  if (call_used_regs[spill_regs[i]])
	    {
	      reg_reloaded_contents[i] = -1;
	      reg_reloaded_insn[i] = 0;
	    }

      /* In case registers overlap, allow certain insns to invalidate
	 particular hard registers.  */

#ifdef INSN_CLOBBERS_REGNO_P
      for (i = 0 ; i < n_spills ; i++)
	if (INSN_CLOBBERS_REGNO_P (insn, spill_regs[i]))
	  {
	    reg_reloaded_contents[i] = -1;
	    reg_reloaded_insn[i] = 0;
	  }
#endif

      insn = next;

#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }
}

/* Discard all record of any value reloaded from X,
   or reloaded in X from someplace else;
   unless X is an output reload reg of the current insn.

   X may be a hard reg (the reload reg)
   or it may be a pseudo reg that was reloaded from.  */

static void
forget_old_reloads_1 (x)
     rtx x;
{
  register int regno;
  int nr;
  int offset = 0;

  /* note_stores does give us subregs of hard regs.  */
  while (GET_CODE (x) == SUBREG)
    {
      offset += SUBREG_WORD (x);
      x = SUBREG_REG (x);
    }

  if (GET_CODE (x) != REG)
    return;

  regno = REGNO (x) + offset;

  if (regno >= FIRST_PSEUDO_REGISTER)
    nr = 1;
  else
    {
      int i;
      nr = HARD_REGNO_NREGS (regno, GET_MODE (x));
      /* Storing into a spilled-reg invalidates its contents.
	 This can happen if a block-local pseudo is allocated to that reg
	 and it wasn't spilled because this block's total need is 0.
	 Then some insn might have an optional reload and use this reg.  */
      for (i = 0; i < nr; i++)
	if (spill_reg_order[regno + i] >= 0
	    /* But don't do this if the reg actually serves as an output
	       reload reg in the current instruction.  */
	    && (n_reloads == 0
		|| ! TEST_HARD_REG_BIT (reg_is_output_reload, regno + i)))
	  {
	    reg_reloaded_contents[spill_reg_order[regno + i]] = -1;
	    reg_reloaded_insn[spill_reg_order[regno + i]] = 0;
	  }
    }

  /* Since value of X has changed,
     forget any value previously copied from it.  */

  while (nr-- > 0)
    /* But don't forget a copy if this is the output reload
       that establishes the copy's validity.  */
    if (n_reloads == 0 || reg_has_output_reload[regno + nr] == 0)
      reg_last_reload_reg[regno + nr] = 0;
}

/* For each reload, the mode of the reload register.  */
static enum machine_mode reload_mode[MAX_RELOADS];

/* For each reload, the largest number of registers it will require.  */
static int reload_nregs[MAX_RELOADS];

/* Comparison function for qsort to decide which of two reloads
   should be handled first.  *P1 and *P2 are the reload numbers.  */

static int
reload_reg_class_lower (p1, p2)
     short *p1, *p2;
{
  register int r1 = *p1, r2 = *p2;
  register int t;

  /* Consider required reloads before optional ones.  */
  t = reload_optional[r1] - reload_optional[r2];
  if (t != 0)
    return t;

  /* Count all solitary classes before non-solitary ones.  */
  t = ((reg_class_size[(int) reload_reg_class[r2]] == 1)
       - (reg_class_size[(int) reload_reg_class[r1]] == 1));
  if (t != 0)
    return t;

  /* Aside from solitaires, consider all multi-reg groups first.  */
  t = reload_nregs[r2] - reload_nregs[r1];
  if (t != 0)
    return t;

  /* Consider reloads in order of increasing reg-class number.  */
  t = (int) reload_reg_class[r1] - (int) reload_reg_class[r2];
  if (t != 0)
    return t;

  /* If reloads are equally urgent, sort by reload number,
     so that the results of qsort leave nothing to chance.  */
  return r1 - r2;
}

/* The following HARD_REG_SETs indicate when each hard register is
   used for a reload of various parts of the current insn.  */

/* If reg is in use as a reload reg for a RELOAD_OTHER reload.  */
static HARD_REG_SET reload_reg_used;
/* If reg is in use for a RELOAD_FOR_INPUT_RELOAD_ADDRESS reload.  */
static HARD_REG_SET reload_reg_used_in_input_addr;
/* If reg is in use for a RELOAD_FOR_OUTPUT_RELOAD_ADDRESS reload.  */
static HARD_REG_SET reload_reg_used_in_output_addr;
/* If reg is in use for a RELOAD_FOR_OPERAND_ADDRESS reload.  */
static HARD_REG_SET reload_reg_used_in_op_addr;
/* If reg is in use for a RELOAD_FOR_INPUT reload.  */
static HARD_REG_SET reload_reg_used_in_input;
/* If reg is in use for a RELOAD_FOR_OUTPUT reload.  */
static HARD_REG_SET reload_reg_used_in_output;

/* If reg is in use as a reload reg for any sort of reload.  */
static HARD_REG_SET reload_reg_used_at_all;

/* Mark reg REGNO as in use for a reload of the sort spec'd by WHEN_NEEDED.
   MODE is used to indicate how many consecutive regs are actually used.  */

static void
mark_reload_reg_in_use (regno, when_needed, mode)
     int regno;
     enum reload_when_needed when_needed;
     enum machine_mode mode;
{
  int nregs = HARD_REGNO_NREGS (regno, mode);
  int i;

  for (i = regno; i < nregs + regno; i++)
    {
      switch (when_needed)
	{
	case RELOAD_OTHER:
	  SET_HARD_REG_BIT (reload_reg_used, i);
	  break;

	case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
	  SET_HARD_REG_BIT (reload_reg_used_in_input_addr, i);
	  break;

	case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
	  SET_HARD_REG_BIT (reload_reg_used_in_output_addr, i);
	  break;

	case RELOAD_FOR_OPERAND_ADDRESS:
	  SET_HARD_REG_BIT (reload_reg_used_in_op_addr, i);
	  break;

	case RELOAD_FOR_INPUT:
	  SET_HARD_REG_BIT (reload_reg_used_in_input, i);
	  break;

	case RELOAD_FOR_OUTPUT:
	  SET_HARD_REG_BIT (reload_reg_used_in_output, i);
	  break;
	}

      SET_HARD_REG_BIT (reload_reg_used_at_all, i);
    }
}

/* 1 if reg REGNO is free as a reload reg for a reload of the sort
   specified by WHEN_NEEDED.  */

static int
reload_reg_free_p (regno, when_needed)
     int regno;
     enum reload_when_needed when_needed;
{
  /* In use for a RELOAD_OTHER means it's not available for anything.  */
  if (TEST_HARD_REG_BIT (reload_reg_used, regno))
    return 0;
  switch (when_needed)
    {
    case RELOAD_OTHER:
      /* In use for anything means not available for a RELOAD_OTHER.  */
      return ! TEST_HARD_REG_BIT (reload_reg_used_at_all, regno);

      /* The other kinds of use can sometimes share a register.  */
    case RELOAD_FOR_INPUT:
      return (! TEST_HARD_REG_BIT (reload_reg_used_in_input, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_op_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_input_addr, regno));
    case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
      return (! TEST_HARD_REG_BIT (reload_reg_used_in_input_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_input, regno));
    case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
      return (! TEST_HARD_REG_BIT (reload_reg_used_in_output_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_output, regno));
    case RELOAD_FOR_OPERAND_ADDRESS:
      return (! TEST_HARD_REG_BIT (reload_reg_used_in_op_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_input, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_output, regno));
    case RELOAD_FOR_OUTPUT:
      return (! TEST_HARD_REG_BIT (reload_reg_used_in_op_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_output_addr, regno)
	      && ! TEST_HARD_REG_BIT (reload_reg_used_in_output, regno));
    }
  abort ();
}

/* Return 1 if the value in reload reg REGNO, as used by a reload
   needed for the part of the insn specified by WHEN_NEEDED,
   is not in use for a reload in any prior part of the insn.

   We can assume that the reload reg was already tested for availability
   at the time it is needed, and we should not check this again,
   in case the reg has already been marked in use.  */

static int
reload_reg_free_before_p (regno, when_needed)
     int regno;
     enum reload_when_needed when_needed;
{
  switch (when_needed)
    {
    case RELOAD_OTHER:
      /* Since a RELOAD_OTHER reload claims the reg for the entire insn,
	 its use starts from the beginning, so nothing can use it earlier.  */
      return 1;

      /* If this use is for part of the insn,
	 check the reg is not in use for any prior part.  */
    case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
      if (TEST_HARD_REG_BIT (reload_reg_used_in_op_addr, regno))
	return 0;
    case RELOAD_FOR_OUTPUT:
      if (TEST_HARD_REG_BIT (reload_reg_used_in_input, regno))
	return 0;
    case RELOAD_FOR_OPERAND_ADDRESS:
      if (TEST_HARD_REG_BIT (reload_reg_used_in_input_addr, regno))
	return 0;
    case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
    case RELOAD_FOR_INPUT:
      return 1;
    }
  abort ();
}

/* Return 1 if the value in reload reg REGNO, as used by a reload
   needed for the part of the insn specified by WHEN_NEEDED,
   is still available in REGNO at the end of the insn.

   We can assume that the reload reg was already tested for availability
   at the time it is needed, and we should not check this again,
   in case the reg has already been marked in use.  */

static int
reload_reg_reaches_end_p (regno, when_needed)
     int regno;
     enum reload_when_needed when_needed;
{
  switch (when_needed)
    {
    case RELOAD_OTHER:
      /* Since a RELOAD_OTHER reload claims the reg for the entire insn,
	 its value must reach the end.  */
      return 1;

      /* If this use is for part of the insn,
	 its value reaches if no subsequent part uses the same register.  */
    case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
    case RELOAD_FOR_INPUT:
      if (TEST_HARD_REG_BIT (reload_reg_used_in_op_addr, regno)
	  || TEST_HARD_REG_BIT (reload_reg_used_in_output, regno))
	return 0;
    case RELOAD_FOR_OPERAND_ADDRESS:
      if (TEST_HARD_REG_BIT (reload_reg_used_in_output_addr, regno))
	return 0;
    case RELOAD_FOR_OUTPUT:
    case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
      return 1;
    }
  abort ();
}

/* Vector of reload-numbers showing the order in which the reloads should
   be processed.  */
short reload_order[MAX_RELOADS];

/* Indexed by reload number, 1 if incoming value
   inherited from previous insns.  */
char reload_inherited[MAX_RELOADS];

/* For an inherited reload, this is the insn the reload was inherited from,
   if we know it.  Otherwise, this is 0.  */
rtx reload_inheritance_insn[MAX_RELOADS];

/* If non-zero, this is a place to get the value of the reload,
   rather than using reload_in.  */
rtx reload_override_in[MAX_RELOADS];

/* For each reload, the index in spill_regs of the spill register used,
   or -1 if we did not need one of the spill registers for this reload.  */
int reload_spill_index[MAX_RELOADS];

/* Index of last register assigned as a spill register.  We allocate in
   a round-robin fashio.  */

static last_spill_reg = 0;

/* Find a spill register to use as a reload register for reload R.
   LAST_RELOAD is non-zero if this is the last reload for the insn being
   processed.

   Set reload_reg_rtx[R] to the register allocated.

   If NOERROR is nonzero, we return 1 if successful,
   or 0 if we couldn't find a spill reg and we didn't change anything.  */

static int
allocate_reload_reg (r, insn, last_reload, noerror)
     int r;
     rtx insn;
     int last_reload;
     int noerror;
{
  int i;
  int pass;
  int count;
  rtx new;
  int regno;

  /* If we put this reload ahead, thinking it is a group,
     then insist on finding a group.  Otherwise we can grab a
     reg that some other reload needs.
     (That can happen when we have a 68000 DATA_OR_FP_REG
     which is a group of data regs or one fp reg.)
     We need not be so restrictive if there are no more reloads
     for this insn.

     ??? Really it would be nicer to have smarter handling
     for that kind of reg class, where a problem like this is normal.
     Perhaps those classes should be avoided for reloading
     by use of more alternatives.  */

  int force_group = reload_nregs[r] > 1 && ! last_reload;

  /* If we want a single register and haven't yet found one,
     take any reg in the right class and not in use.
     If we want a consecutive group, here is where we look for it.

     We use two passes so we can first look for reload regs to
     reuse, which are already in use for other reloads in this insn,
     and only then use additional registers.
     I think that maximizing reuse is needed to make sure we don't
     run out of reload regs.  Suppose we have three reloads, and
     reloads A and B can share regs.  These need two regs.
     Suppose A and B are given different regs.
     That leaves none for C.  */
  for (pass = 0; pass < 2; pass++)
    {
      /* I is the index in spill_regs.
	 We advance it round-robin between insns to use all spill regs
	 equally, so that inherited reloads have a chance
	 of leapfrogging each other.  */

      for (count = 0, i = last_spill_reg; count < n_spills; count++)
	{
	  int class = (int) reload_reg_class[r];

	  i = (i + 1) % n_spills;

	  if (reload_reg_free_p (spill_regs[i], reload_when_needed[r])
	      && TEST_HARD_REG_BIT (reg_class_contents[class], spill_regs[i])
	      && HARD_REGNO_MODE_OK (spill_regs[i], reload_mode[r])
	      /* Look first for regs to share, then for unshared.  */
	      && (pass || TEST_HARD_REG_BIT (reload_reg_used_at_all,
					     spill_regs[i])))
	    {
	      int nr = HARD_REGNO_NREGS (spill_regs[i], reload_mode[r]);
	      /* Avoid the problem where spilling a GENERAL_OR_FP_REG
		 (on 68000) got us two FP regs.  If NR is 1,
		 we would reject both of them.  */
	      if (force_group)
		nr = CLASS_MAX_NREGS (reload_reg_class[r], reload_mode[r]);
	      /* If we need only one reg, we have already won.  */
	      if (nr == 1)
		{
		  /* But reject a single reg if we demand a group.  */
		  if (force_group)
		    continue;
		  break;
		}
	      /* Otherwise check that as many consecutive regs as we need
		 are available here.
		 Also, don't use for a group registers that are
		 needed for nongroups.  */
	      if (! TEST_HARD_REG_BIT (counted_for_nongroups, spill_regs[i]))
		while (nr > 1)
		  {
		    regno = spill_regs[i] + nr - 1;
		    if (!(TEST_HARD_REG_BIT (reg_class_contents[class], regno)
			  && spill_reg_order[regno] >= 0
			  && reload_reg_free_p (regno, reload_when_needed[r])
			  && ! TEST_HARD_REG_BIT (counted_for_nongroups,
						  regno)))
		      break;
		    nr--;
		  }
	      if (nr == 1)
		break;
	    }
	}

      /* If we found something on pass 1, omit pass 2.  */
      if (count < n_spills)
	break;
    }

  /* We should have found a spill register by now.  */
  if (count == n_spills)
    {
      if (noerror)
	return 0;
      goto failure;
    }

  last_spill_reg = i;

  /* Mark as in use for this insn the reload regs we use for this.  */
  mark_reload_reg_in_use (spill_regs[i], reload_when_needed[r],
			  reload_mode[r]);

  new = spill_reg_rtx[i];

  if (new == 0 || GET_MODE (new) != reload_mode[r])
    spill_reg_rtx[i] = new = gen_rtx (REG, reload_mode[r], spill_regs[i]);

  reload_reg_rtx[r] = new;
  reload_spill_index[r] = i;
  regno = true_regnum (new);

  /* Detect when the reload reg can't hold the reload mode.
     This used to be one `if', but Sequent compiler can't handle that.  */
  if (HARD_REGNO_MODE_OK (regno, reload_mode[r]))
    {
      enum machine_mode test_mode = VOIDmode;
      if (reload_in[r])
	test_mode = GET_MODE (reload_in[r]);
      /* If reload_in[r] has VOIDmode, it means we will load it
	 in whatever mode the reload reg has: to wit, reload_mode[r].
	 We have already tested that for validity.  */
      /* Aside from that, we need to test that the expressions
	 to reload from or into have modes which are valid for this
	 reload register.  Otherwise the reload insns would be invalid.  */
      if (! (reload_in[r] != 0 && test_mode != VOIDmode
	     && ! HARD_REGNO_MODE_OK (regno, test_mode)))
	if (! (reload_out[r] != 0
	       && ! HARD_REGNO_MODE_OK (regno, GET_MODE (reload_out[r]))))
	  /* The reg is OK.  */
	  return 1;
    }

  /* The reg is not OK.  */
  if (noerror)
    return 0;

 failure:
  if (asm_noperands (PATTERN (insn)) < 0)
    /* It's the compiler's fault.  */
    abort ();

  /* It's the user's fault; the operand's mode and constraint
     don't match.  Disable this reload so we don't crash in final.  */
  error_for_asm (insn,
		 "`asm' operand constraint incompatible with operand size");
  reload_in[r] = 0;
  reload_out[r] = 0;
  reload_reg_rtx[r] = 0;
  reload_optional[r] = 1;
  reload_secondary_p[r] = 1;

  return 1;
}

/* Assign hard reg targets for the pseudo-registers we must reload
   into hard regs for this insn.
   Also output the instructions to copy them in and out of the hard regs.

   For machines with register classes, we are responsible for
   finding a reload reg in the proper class.  */

static void
choose_reload_regs (insn, avoid_return_reg)
     rtx insn;
     /* This argument is currently ignored.  */
     rtx avoid_return_reg;
{
  register int i, j;
  int max_group_size = 1;
  enum reg_class group_class = NO_REGS;
  int inheritance;

  rtx save_reload_reg_rtx[MAX_RELOADS];
  char save_reload_inherited[MAX_RELOADS];
  rtx save_reload_inheritance_insn[MAX_RELOADS];
  rtx save_reload_override_in[MAX_RELOADS];
  int save_reload_spill_index[MAX_RELOADS];
  HARD_REG_SET save_reload_reg_used;
  HARD_REG_SET save_reload_reg_used_in_input_addr;
  HARD_REG_SET save_reload_reg_used_in_output_addr;
  HARD_REG_SET save_reload_reg_used_in_op_addr;
  HARD_REG_SET save_reload_reg_used_in_input;
  HARD_REG_SET save_reload_reg_used_in_output;
  HARD_REG_SET save_reload_reg_used_at_all;

  bzero (reload_inherited, MAX_RELOADS);
  bzero (reload_inheritance_insn, MAX_RELOADS * sizeof (rtx));
  bzero (reload_override_in, MAX_RELOADS * sizeof (rtx));

  CLEAR_HARD_REG_SET (reload_reg_used);
  CLEAR_HARD_REG_SET (reload_reg_used_at_all);
  CLEAR_HARD_REG_SET (reload_reg_used_in_input_addr);
  CLEAR_HARD_REG_SET (reload_reg_used_in_output_addr);
  CLEAR_HARD_REG_SET (reload_reg_used_in_op_addr);
  CLEAR_HARD_REG_SET (reload_reg_used_in_output);
  CLEAR_HARD_REG_SET (reload_reg_used_in_input);

  /* Distinguish output-only and input-only reloads
     because they can overlap with other things.  */
  for (j = 0; j < n_reloads; j++)
    if (reload_when_needed[j] == RELOAD_OTHER
	&& ! reload_needed_for_multiple[j])
      {
	if (reload_in[j] == 0)
	  {
	    /* But earlyclobber operands must stay as RELOAD_OTHER.  */
	    for (i = 0; i < n_earlyclobbers; i++)
	      if (rtx_equal_p (reload_out[j], reload_earlyclobbers[i]))
		break;
	    if (i == n_earlyclobbers)
	      reload_when_needed[j] = RELOAD_FOR_OUTPUT;
	  }
	if (reload_out[j] == 0)
	  reload_when_needed[j] = RELOAD_FOR_INPUT;

	if (reload_secondary_reload[j] >= 0
	    && ! reload_needed_for_multiple[reload_secondary_reload[j]])
	  reload_when_needed[reload_secondary_reload[j]]
	    = reload_when_needed[j];
      }

#ifdef SMALL_REGISTER_CLASSES
  /* Don't bother with avoiding the return reg
     if we have no mandatory reload that could use it.  */
  if (avoid_return_reg)
    {
      int do_avoid = 0;
      int regno = REGNO (avoid_return_reg);
      int nregs
	= HARD_REGNO_NREGS (regno, GET_MODE (avoid_return_reg));
      int r;

      for (r = regno; r < regno + nregs; r++)
	if (spill_reg_order[r] >= 0)
	  for (j = 0; j < n_reloads; j++)
	    if (!reload_optional[j] && reload_reg_rtx[j] == 0
		&& (reload_in[j] != 0 || reload_out[j] != 0
		    || reload_secondary_p[j])
		&&
		TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[j]], r))
	      do_avoid = 1;
      if (!do_avoid)
	avoid_return_reg = 0;
    }
#endif /* SMALL_REGISTER_CLASSES */

#if 0  /* Not needed, now that we can always retry without inheritance.  */
  /* See if we have more mandatory reloads than spill regs.
     If so, then we cannot risk optimizations that could prevent
     reloads from sharing one spill register.

     Since we will try finding a better register than reload_reg_rtx
     unless it is equal to reload_in or reload_out, count such reloads.  */

  {
    int tem = 0;
#ifdef SMALL_REGISTER_CLASSES
    int tem = (avoid_return_reg != 0);
#endif
    for (j = 0; j < n_reloads; j++)
      if (! reload_optional[j]
	  && (reload_in[j] != 0 || reload_out[j] != 0 || reload_secondary_p[j])
	  && (reload_reg_rtx[j] == 0
	      || (! rtx_equal_p (reload_reg_rtx[j], reload_in[j])
		  && ! rtx_equal_p (reload_reg_rtx[j], reload_out[j]))))
	tem++;
    if (tem > n_spills)
      must_reuse = 1;
  }
#endif

#ifdef SMALL_REGISTER_CLASSES
  /* Don't use the subroutine call return reg for a reload
     if we are supposed to avoid it.  */
  if (avoid_return_reg)
    {
      int regno = REGNO (avoid_return_reg);
      int nregs
	= HARD_REGNO_NREGS (regno, GET_MODE (avoid_return_reg));
      int r;

      for (r = regno; r < regno + nregs; r++)
	if (spill_reg_order[r] >= 0)
	  SET_HARD_REG_BIT (reload_reg_used, r);
    }
#endif /* SMALL_REGISTER_CLASSES */

  /* In order to be certain of getting the registers we need,
     we must sort the reloads into order of increasing register class.
     Then our grabbing of reload registers will parallel the process
     that provided the reload registers.

     Also note whether any of the reloads wants a consecutive group of regs.
     If so, record the maximum size of the group desired and what
     register class contains all the groups needed by this insn.  */

  for (j = 0; j < n_reloads; j++)
    {
      reload_order[j] = j;
      reload_spill_index[j] = -1;

      reload_mode[j]
	= (reload_strict_low[j] && reload_out[j]
	   ? GET_MODE (SUBREG_REG (reload_out[j]))
	   : (reload_inmode[j] == VOIDmode
	      || (GET_MODE_SIZE (reload_outmode[j])
		  > GET_MODE_SIZE (reload_inmode[j])))
	   ? reload_outmode[j] : reload_inmode[j]);

      reload_nregs[j] = CLASS_MAX_NREGS (reload_reg_class[j], reload_mode[j]);

      if (reload_nregs[j] > 1)
	{
	  max_group_size = MAX (reload_nregs[j], max_group_size);
	  group_class = reg_class_superunion[(int)reload_reg_class[j]][(int)group_class];
	}

      /* If we have already decided to use a certain register,
	 don't use it in another way.  */
      if (reload_reg_rtx[j])
	mark_reload_reg_in_use (REGNO (reload_reg_rtx[j]),
				reload_when_needed[j], reload_mode[j]);
    }

  if (n_reloads > 1)
    qsort (reload_order, n_reloads, sizeof (short), reload_reg_class_lower);

  bcopy (reload_reg_rtx, save_reload_reg_rtx, sizeof reload_reg_rtx);
  bcopy (reload_inherited, save_reload_inherited, sizeof reload_inherited);
  bcopy (reload_inheritance_insn, save_reload_inheritance_insn,
	 sizeof reload_inheritance_insn);
  bcopy (reload_override_in, save_reload_override_in,
	 sizeof reload_override_in);
  bcopy (reload_spill_index, save_reload_spill_index,
	 sizeof reload_spill_index);
  COPY_HARD_REG_SET (save_reload_reg_used, reload_reg_used);
  COPY_HARD_REG_SET (save_reload_reg_used_at_all, reload_reg_used_at_all);
  COPY_HARD_REG_SET (save_reload_reg_used_in_output,
		     reload_reg_used_in_output);
  COPY_HARD_REG_SET (save_reload_reg_used_in_input,
		     reload_reg_used_in_input);
  COPY_HARD_REG_SET (save_reload_reg_used_in_input_addr,
		     reload_reg_used_in_input_addr);
  COPY_HARD_REG_SET (save_reload_reg_used_in_output_addr,
		     reload_reg_used_in_output_addr);
  COPY_HARD_REG_SET (save_reload_reg_used_in_op_addr,
		     reload_reg_used_in_op_addr);

  /* If -O, try first with inheritance, then turning it off.
     If not -O, don't do inheritance.
     Using inheritance when not optimizing leads to paradoxes
     with fp on the 68k: fp numbers (not NaNs) fail to be equal to themselves
     because one side of the comparison might be inherited.  */

  for (inheritance = optimize > 0; inheritance >= 0; inheritance--)
    {
      /* Process the reloads in order of preference just found.
	 Beyond this point, subregs can be found in reload_reg_rtx.

	 This used to look for an existing reloaded home for all
	 of the reloads, and only then perform any new reloads.
	 But that could lose if the reloads were done out of reg-class order
	 because a later reload with a looser constraint might have an old
	 home in a register needed by an earlier reload with a tighter constraint.

	 To solve this, we make two passes over the reloads, in the order
	 described above.  In the first pass we try to inherit a reload
	 from a previous insn.  If there is a later reload that needs a
	 class that is a proper subset of the class being processed, we must
	 also allocate a spill register during the first pass.

	 Then make a second pass over the reloads to allocate any reloads
	 that haven't been given registers yet.  */

      for (j = 0; j < n_reloads; j++)
	{
	  register int r = reload_order[j];

	  /* Ignore reloads that got marked inoperative.  */
	  if (reload_out[r] == 0 && reload_in[r] == 0 && ! reload_secondary_p[r])
	    continue;

	  /* If find_reloads chose a to use reload_in or reload_out as a reload
	     register, we don't need to chose one.  Otherwise, try even if it found
	     one since we might save an insn if we find the value lying around.  */
	  if (reload_in[r] != 0 && reload_reg_rtx[r] != 0
	      && (rtx_equal_p (reload_in[r], reload_reg_rtx[r])
		  || rtx_equal_p (reload_out[r], reload_reg_rtx[r])))
	    continue;

#if 0 /* No longer needed for correct operation.
	 It might give better code, or might not; worth an experiment?  */
	  /* If this is an optional reload, we can't inherit from earlier insns
	     until we are sure that any non-optional reloads have been allocated.
	     The following code takes advantage of the fact that optional reloads
	     are at the end of reload_order.  */
	  if (reload_optional[r] != 0)
	    for (i = 0; i < j; i++)
	      if ((reload_out[reload_order[i]] != 0
		   || reload_in[reload_order[i]] != 0
		   || reload_secondary_p[reload_order[i]])
		  && ! reload_optional[reload_order[i]]
		  && reload_reg_rtx[reload_order[i]] == 0)
		allocate_reload_reg (reload_order[i], insn, 0, inheritance);
#endif

	  /* First see if this pseudo is already available as reloaded
	     for a previous insn.  We cannot try to inherit for reloads
	     that are smaller than the maximum number of registers needed
	     for groups unless the register we would allocate cannot be used
	     for the groups.

	     We could check here to see if this is a secondary reload for
	     an object that is already in a register of the desired class.
	     This would avoid the need for the secondary reload register.
	     But this is complex because we can't easily determine what
	     objects might want to be loaded via this reload.  So let a register
	     be allocated here.  In `emit_reload_insns' we suppress one of the
	     loads in the case described above.  */

	  if (inheritance)
	    {
	      register int regno = -1;
	      enum machine_mode mode;

	      if (reload_in[r] == 0)
		;
	      else if (GET_CODE (reload_in[r]) == REG)
		{
		  regno = REGNO (reload_in[r]);
		  mode = GET_MODE (reload_in[r]);
		}
	      else if (GET_CODE (reload_in_reg[r]) == REG)
		{
		  regno = REGNO (reload_in_reg[r]);
		  mode = GET_MODE (reload_in_reg[r]);
		}
#if 0
	      /* This won't work, since REGNO can be a pseudo reg number.
		 Also, it takes much more hair to keep track of all the things
		 that can invalidate an inherited reload of part of a pseudoreg.  */
	      else if (GET_CODE (reload_in[r]) == SUBREG
		       && GET_CODE (SUBREG_REG (reload_in[r])) == REG)
		regno = REGNO (SUBREG_REG (reload_in[r])) + SUBREG_WORD (reload_in[r]);
#endif

	      if (regno >= 0 && reg_last_reload_reg[regno] != 0)
		{
		  i = spill_reg_order[REGNO (reg_last_reload_reg[regno])];

		  if (reg_reloaded_contents[i] == regno
		      && (GET_MODE_SIZE (GET_MODE (reg_last_reload_reg[regno]))
			  >= GET_MODE_SIZE (mode))
		      && HARD_REGNO_MODE_OK (spill_regs[i], reload_mode[r])
		      && TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[r]],
					    spill_regs[i])
		      && (reload_nregs[r] == max_group_size
			  || ! TEST_HARD_REG_BIT (reg_class_contents[(int) group_class],
						  spill_regs[i]))
		      && reload_reg_free_p (spill_regs[i], reload_when_needed[r])
		      && reload_reg_free_before_p (spill_regs[i],
						   reload_when_needed[r]))
		    {
		      /* If a group is needed, verify that all the subsequent
			 registers still have their values intact. */
		      int nr
			= HARD_REGNO_NREGS (spill_regs[i], reload_mode[r]);
		      int k;

		      for (k = 1; k < nr; k++)
			if (reg_reloaded_contents[spill_reg_order[spill_regs[i] + k]]
			    != regno)
			  break;

		      if (k == nr)
			{
			  /* Mark the register as in use for this part of
			     the insn.  */
			  mark_reload_reg_in_use (spill_regs[i],
						  reload_when_needed[r],
						  reload_mode[r]);
			  reload_reg_rtx[r] = reg_last_reload_reg[regno];
			  reload_inherited[r] = 1;
			  reload_inheritance_insn[r] = reg_reloaded_insn[i];
			  reload_spill_index[r] = i;
			}
		    }
		}
	    }

	  /* Here's another way to see if the value is already lying around.  */
	  if (inheritance
	      && reload_in[r] != 0
	      && ! reload_inherited[r]
	      && reload_out[r] == 0
	      && (CONSTANT_P (reload_in[r])
		  || GET_CODE (reload_in[r]) == PLUS
		  || GET_CODE (reload_in[r]) == REG
		  || GET_CODE (reload_in[r]) == MEM)
	      && (reload_nregs[r] == max_group_size
		  || ! reg_classes_intersect_p (reload_reg_class[r], group_class)))
	    {
	      register rtx equiv
		= find_equiv_reg (reload_in[r], insn, reload_reg_class[r],
				  -1, NULL_PTR, 0, reload_mode[r]);
	      int regno;

	      if (equiv != 0)
		{
		  if (GET_CODE (equiv) == REG)
		    regno = REGNO (equiv);
		  else if (GET_CODE (equiv) == SUBREG)
		    {
		      regno = REGNO (SUBREG_REG (equiv));
		      if (regno < FIRST_PSEUDO_REGISTER)
			regno += SUBREG_WORD (equiv);
		    }
		  else
		    abort ();
		}

	      /* If we found a spill reg, reject it unless it is free
		 and of the desired class.  */
	      if (equiv != 0
		  && ((spill_reg_order[regno] >= 0
		       && ! reload_reg_free_before_p (regno,
						      reload_when_needed[r]))
		      || ! TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[r]],
					      regno)))
		equiv = 0;

	      if (equiv != 0 && TEST_HARD_REG_BIT (reload_reg_used_at_all, regno))
		equiv = 0;

	      if (equiv != 0 && ! HARD_REGNO_MODE_OK (regno, reload_mode[r]))
		equiv = 0;

	      /* We found a register that contains the value we need.
		 If this register is the same as an `earlyclobber' operand
		 of the current insn, just mark it as a place to reload from
		 since we can't use it as the reload register itself.  */

	      if (equiv != 0)
		for (i = 0; i < n_earlyclobbers; i++)
		  if (reg_overlap_mentioned_for_reload_p (equiv,
							  reload_earlyclobbers[i]))
		    {
		      reload_override_in[r] = equiv;
		      equiv = 0;
		      break;
		    }

	      /* JRV: If the equiv register we have found is explicitly
		 clobbered in the current insn, mark but don't use, as above. */

	      if (equiv != 0 && regno_clobbered_p (regno, insn))
		{
		  reload_override_in[r] = equiv;
		  equiv = 0;
		}

	      /* If we found an equivalent reg, say no code need be generated
		 to load it, and use it as our reload reg.  */
	      if (equiv != 0 && regno != FRAME_POINTER_REGNUM)
		{
		  reload_reg_rtx[r] = equiv;
		  reload_inherited[r] = 1;
		  /* If it is a spill reg,
		     mark the spill reg as in use for this insn.  */
		  i = spill_reg_order[regno];
		  if (i >= 0)
		    mark_reload_reg_in_use (regno, reload_when_needed[r],
					    reload_mode[r]);
		}
	    }

	  /* If we found a register to use already, or if this is an optional
	     reload, we are done.  */
	  if (reload_reg_rtx[r] != 0 || reload_optional[r] != 0)
	    continue;

#if 0 /* No longer needed for correct operation.  Might or might not
	 give better code on the average.  Want to experiment?  */

	  /* See if there is a later reload that has a class different from our
	     class that intersects our class or that requires less register
	     than our reload.  If so, we must allocate a register to this
	     reload now, since that reload might inherit a previous reload
	     and take the only available register in our class.  Don't do this
	     for optional reloads since they will force all previous reloads
	     to be allocated.  Also don't do this for reloads that have been
	     turned off.  */

	  for (i = j + 1; i < n_reloads; i++)
	    {
	      int s = reload_order[i];

	      if ((reload_in[s] == 0 && reload_out[s] == 0
		   && ! reload_secondary_p[s])
		  || reload_optional[s])
		continue;

	      if ((reload_reg_class[s] != reload_reg_class[r]
		   && reg_classes_intersect_p (reload_reg_class[r],
					       reload_reg_class[s]))
		  || reload_nregs[s] < reload_nregs[r])
	      break;
	    }

	  if (i == n_reloads)
	    continue;

	  allocate_reload_reg (r, insn, j == n_reloads - 1, inheritance);
#endif
	}

      /* Now allocate reload registers for anything non-optional that
	 didn't get one yet.  */
      for (j = 0; j < n_reloads; j++)
	{
	  register int r = reload_order[j];

	  /* Ignore reloads that got marked inoperative.  */
	  if (reload_out[r] == 0 && reload_in[r] == 0 && ! reload_secondary_p[r])
	    continue;

	  /* Skip reloads that already have a register allocated or are
	     optional. */
	  if (reload_reg_rtx[r] != 0 || reload_optional[r])
	    continue;

	  if (! allocate_reload_reg (r, insn, j == n_reloads - 1, inheritance))
	    break;
	}

      /* If that loop got all the way, we have won.  */
      if (j == n_reloads)
	break;

    fail:
      /* Loop around and try without any inheritance.  */
      /* First undo everything done by the failed attempt
	 to allocate with inheritance.  */
      bcopy (save_reload_reg_rtx, reload_reg_rtx, sizeof reload_reg_rtx);
      bcopy (save_reload_inherited, reload_inherited, sizeof reload_inherited);
      bcopy (save_reload_inheritance_insn, reload_inheritance_insn,
	     sizeof reload_inheritance_insn);
      bcopy (save_reload_override_in, reload_override_in,
	     sizeof reload_override_in);
      bcopy (save_reload_spill_index, reload_spill_index,
	     sizeof reload_spill_index);
      COPY_HARD_REG_SET (reload_reg_used, save_reload_reg_used);
      COPY_HARD_REG_SET (reload_reg_used_at_all, save_reload_reg_used_at_all);
      COPY_HARD_REG_SET (reload_reg_used_in_input,
			 save_reload_reg_used_in_input);
      COPY_HARD_REG_SET (reload_reg_used_in_output,
			 save_reload_reg_used_in_output);
      COPY_HARD_REG_SET (reload_reg_used_in_input_addr,
			 save_reload_reg_used_in_input_addr);
      COPY_HARD_REG_SET (reload_reg_used_in_output_addr,
			 save_reload_reg_used_in_output_addr);
      COPY_HARD_REG_SET (reload_reg_used_in_op_addr,
			 save_reload_reg_used_in_op_addr);
    }

  /* If we thought we could inherit a reload, because it seemed that
     nothing else wanted the same reload register earlier in the insn,
     verify that assumption, now that all reloads have been assigned.  */

  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];

      if (reload_inherited[r] && reload_reg_rtx[r] != 0
	  && ! reload_reg_free_before_p (true_regnum (reload_reg_rtx[r]),
					 reload_when_needed[r]))
	reload_inherited[r] = 0;

      /* If we found a better place to reload from,
	 validate it in the same fashion, if it is a reload reg.  */
      if (reload_override_in[r]
	  && (GET_CODE (reload_override_in[r]) == REG
	      || GET_CODE (reload_override_in[r]) == SUBREG))
	{
	  int regno = true_regnum (reload_override_in[r]);
	  if (spill_reg_order[regno] >= 0
	      && ! reload_reg_free_before_p (regno, reload_when_needed[r]))
	    reload_override_in[r] = 0;
	}
    }

  /* Now that reload_override_in is known valid,
     actually override reload_in.  */
  for (j = 0; j < n_reloads; j++)
    if (reload_override_in[j])
      reload_in[j] = reload_override_in[j];

  /* If this reload won't be done because it has been cancelled or is
     optional and not inherited, clear reload_reg_rtx so other
     routines (such as subst_reloads) don't get confused.  */
  for (j = 0; j < n_reloads; j++)
    if ((reload_optional[j] && ! reload_inherited[j])
	|| (reload_in[j] == 0 && reload_out[j] == 0
	    && ! reload_secondary_p[j]))
      reload_reg_rtx[j] = 0;

  /* Record which pseudos and which spill regs have output reloads.  */
  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];

      i = reload_spill_index[r];

      /* I is nonneg if this reload used one of the spill regs.
	 If reload_reg_rtx[r] is 0, this is an optional reload
	 that we opted to ignore.  */
      if (reload_out[r] != 0 && GET_CODE (reload_out[r]) == REG
	  && reload_reg_rtx[r] != 0)
	{
	  register int nregno = REGNO (reload_out[r]);
	  int nr = 1;

	  if (nregno < FIRST_PSEUDO_REGISTER)
	    nr = HARD_REGNO_NREGS (nregno, reload_mode[r]);

	  while (--nr >= 0)
	    reg_has_output_reload[nregno + nr] = 1;

	  if (i >= 0)
	    {
	      nr = HARD_REGNO_NREGS (spill_regs[i], reload_mode[r]);
	      while (--nr >= 0)
		SET_HARD_REG_BIT (reg_is_output_reload, spill_regs[i] + nr);
	    }

	  if (reload_when_needed[r] != RELOAD_OTHER
	      && reload_when_needed[r] != RELOAD_FOR_OUTPUT)
	    abort ();
	}
    }
}

/* Output insns to reload values in and out of the chosen reload regs.  */

static void
emit_reload_insns (insn)
     rtx insn;
{
  register int j;
  rtx following_insn = NEXT_INSN (insn);
  rtx before_insn = insn;
  rtx first_output_reload_insn = NEXT_INSN (insn);
  rtx first_other_reload_insn = insn;
  rtx first_operand_address_reload_insn = insn;
  int special;
  /* Values to be put in spill_reg_store are put here first.  */
  rtx new_spill_reg_store[FIRST_PSEUDO_REGISTER];

  /* If this is a CALL_INSN preceded by USE insns, any reload insns
     must go in front of the first USE insn, not in front of INSN.  */

  if (GET_CODE (insn) == CALL_INSN && GET_CODE (PREV_INSN (insn)) == INSN
      && GET_CODE (PATTERN (PREV_INSN (insn))) == USE)
    while (GET_CODE (PREV_INSN (before_insn)) == INSN
	   && GET_CODE (PATTERN (PREV_INSN (before_insn))) == USE)
      first_other_reload_insn = first_operand_address_reload_insn
	= before_insn = PREV_INSN (before_insn);

  /* Now output the instructions to copy the data into and out of the
     reload registers.  Do these in the order that the reloads were reported,
     since reloads of base and index registers precede reloads of operands
     and the operands may need the base and index registers reloaded.  */

  for (j = 0; j < n_reloads; j++)
    {
      register rtx old;
      rtx oldequiv_reg = 0;
      rtx this_reload_insn = 0;
      rtx store_insn = 0;

      old = reload_in[j];
      if (old != 0 && ! reload_inherited[j]
	  && ! rtx_equal_p (reload_reg_rtx[j], old)
	  && reload_reg_rtx[j] != 0)
	{
	  register rtx reloadreg = reload_reg_rtx[j];
	  rtx oldequiv = 0;
	  enum machine_mode mode;
	  rtx where;
	  rtx reload_insn;

	  /* Determine the mode to reload in.
	     This is very tricky because we have three to choose from.
	     There is the mode the insn operand wants (reload_inmode[J]).
	     There is the mode of the reload register RELOADREG.
	     There is the intrinsic mode of the operand, which we could find
	     by stripping some SUBREGs.
	     It turns out that RELOADREG's mode is irrelevant:
	     we can change that arbitrarily.

	     Consider (SUBREG:SI foo:QI) as an operand that must be SImode;
	     then the reload reg may not support QImode moves, so use SImode.
	     If foo is in memory due to spilling a pseudo reg, this is safe,
	     because the QImode value is in the least significant part of a
	     slot big enough for a SImode.  If foo is some other sort of
	     memory reference, then it is impossible to reload this case,
	     so previous passes had better make sure this never happens.

	     Then consider a one-word union which has SImode and one of its
	     members is a float, being fetched as (SUBREG:SF union:SI).
	     We must fetch that as SFmode because we could be loading into
	     a float-only register.  In this case OLD's mode is correct.

	     Consider an immediate integer: it has VOIDmode.  Here we need
	     to get a mode from something else.

	     In some cases, there is a fourth mode, the operand's
	     containing mode.  If the insn specifies a containing mode for
	     this operand, it overrides all others.

	     I am not sure whether the algorithm here is always right,
	     but it does the right things in those cases.  */

	  mode = GET_MODE (old);
	  if (mode == VOIDmode)
	    mode = reload_inmode[j];
	  if (reload_strict_low[j])
	    mode = GET_MODE (SUBREG_REG (reload_in[j]));

#ifdef SECONDARY_INPUT_RELOAD_CLASS
	  /* If we need a secondary register for this operation, see if
	     the value is already in a register in that class.  Don't
	     do this if the secondary register will be used as a scratch
	     register.  */

	  if (reload_secondary_reload[j] >= 0
	      && reload_secondary_icode[j] == CODE_FOR_nothing
	      && optimize)
	    oldequiv
	      = find_equiv_reg (old, insn,
				reload_reg_class[reload_secondary_reload[j]],
				-1, NULL_PTR, 0, mode);
#endif

	  /* If reloading from memory, see if there is a register
	     that already holds the same value.  If so, reload from there.
	     We can pass 0 as the reload_reg_p argument because
	     any other reload has either already been emitted,
	     in which case find_equiv_reg will see the reload-insn,
	     or has yet to be emitted, in which case it doesn't matter
	     because we will use this equiv reg right away.  */

	  if (oldequiv == 0 && optimize
	      && (GET_CODE (old) == MEM
		  || (GET_CODE (old) == REG
		      && REGNO (old) >= FIRST_PSEUDO_REGISTER
		      && reg_renumber[REGNO (old)] < 0)))
	    oldequiv = find_equiv_reg (old, insn, GENERAL_REGS,
				       -1, NULL_PTR, 0, mode);

	  if (oldequiv)
	    {
	      int regno = true_regnum (oldequiv);

	      /* If OLDEQUIV is a spill register, don't use it for this
		 if any other reload needs it at an earlier stage of this insn
		 or at this stage.  */
	      if (spill_reg_order[regno] >= 0
		  && (! reload_reg_free_p (regno, reload_when_needed[j])
		      || ! reload_reg_free_before_p (regno,
						     reload_when_needed[j])))
		oldequiv = 0;

	      /* If OLDEQUIV is not a spill register,
		 don't use it if any other reload wants it.  */
	      if (spill_reg_order[regno] < 0)
		{
		  int k;
		  for (k = 0; k < n_reloads; k++)
		    if (reload_reg_rtx[k] != 0 && k != j
			&& reg_overlap_mentioned_for_reload_p (reload_reg_rtx[k],
							       oldequiv))
		      {
			oldequiv = 0;
			break;
		      }
		}
	    }

	  if (oldequiv == 0)
	    oldequiv = old;
	  else if (GET_CODE (oldequiv) == REG)
	    oldequiv_reg = oldequiv;
	  else if (GET_CODE (oldequiv) == SUBREG)
	    oldequiv_reg = SUBREG_REG (oldequiv);

	  /* Encapsulate both RELOADREG and OLDEQUIV into that mode,
	     then load RELOADREG from OLDEQUIV.  */

	  if (GET_MODE (reloadreg) != mode)
	    reloadreg = gen_rtx (REG, mode, REGNO (reloadreg));
	  while (GET_CODE (oldequiv) == SUBREG && GET_MODE (oldequiv) != mode)
	    oldequiv = SUBREG_REG (oldequiv);
	  if (GET_MODE (oldequiv) != VOIDmode
	      && mode != GET_MODE (oldequiv))
	    oldequiv = gen_rtx (SUBREG, mode, oldequiv, 0);

	  /* Decide where to put reload insn for this reload.  */
	  switch (reload_when_needed[j])
	    {
	    case RELOAD_FOR_INPUT:
	    case RELOAD_OTHER:
	      where = first_operand_address_reload_insn;
	      break;
	    case RELOAD_FOR_INPUT_RELOAD_ADDRESS:
	      where = first_other_reload_insn;
	      break;
	    case RELOAD_FOR_OUTPUT_RELOAD_ADDRESS:
	      where = first_output_reload_insn;
	      break;
	    case RELOAD_FOR_OPERAND_ADDRESS:
	      where = before_insn;
	    }

	  special = 0;

	  /* Auto-increment addresses must be reloaded in a special way.  */
	  if (GET_CODE (oldequiv) == POST_INC
	      || GET_CODE (oldequiv) == POST_DEC
	      || GET_CODE (oldequiv) == PRE_INC
	      || GET_CODE (oldequiv) == PRE_DEC)
	    {
	      /* We are not going to bother supporting the case where a
		 incremented register can't be copied directly from
		 OLDEQUIV since this seems highly unlikely.  */
	      if (reload_secondary_reload[j] >= 0)
		abort ();
	      /* Prevent normal processing of this reload.  */
	      special = 1;
	      /* Output a special code sequence for this case.  */
	      this_reload_insn
		= inc_for_reload (reloadreg, oldequiv, reload_inc[j], where);
	    }

	  /* If we are reloading a pseudo-register that was set by the previous
	     insn, see if we can get rid of that pseudo-register entirely
	     by redirecting the previous insn into our reload register.  */

	  else if (optimize && GET_CODE (old) == REG
		   && REGNO (old) >= FIRST_PSEUDO_REGISTER
		   && dead_or_set_p (insn, old)
		   /* This is unsafe if some other reload
		      uses the same reg first.  */
		   && (reload_when_needed[j] == RELOAD_OTHER
		       || reload_when_needed[j] == RELOAD_FOR_INPUT
		       || reload_when_needed[j] == RELOAD_FOR_INPUT_RELOAD_ADDRESS))
	    {
	      rtx temp = PREV_INSN (insn);
	      while (temp && GET_CODE (temp) == NOTE)
		temp = PREV_INSN (temp);
	      if (temp
		  && GET_CODE (temp) == INSN
		  && GET_CODE (PATTERN (temp)) == SET
		  && SET_DEST (PATTERN (temp)) == old
		  /* Make sure we can access insn_operand_constraint.  */
		  && asm_noperands (PATTERN (temp)) < 0
		  /* This is unsafe if prev insn rejects our reload reg.  */
		  && constraint_accepts_reg_p (insn_operand_constraint[recog_memoized (temp)][0],
					       reloadreg)
		  /* This is unsafe if operand occurs more than once in current
		     insn.  Perhaps some occurrences aren't reloaded.  */
		  && count_occurrences (PATTERN (insn), old) == 1
		  /* Don't risk splitting a matching pair of operands.  */
		  && ! reg_mentioned_p (old, SET_SRC (PATTERN (temp))))
		{
		  /* Store into the reload register instead of the pseudo.  */
		  SET_DEST (PATTERN (temp)) = reloadreg;
		  /* If these are the only uses of the pseudo reg,
		     pretend for GDB it lives in the reload reg we used.  */
		  if (reg_n_deaths[REGNO (old)] == 1
		      && reg_n_sets[REGNO (old)] == 1)
		    {
		      reg_renumber[REGNO (old)] = REGNO (reload_reg_rtx[j]);
		      alter_reg (REGNO (old), -1);
		    }
		  special = 1;
		}
	    }

	  /* We can't do that, so output an insn to load RELOADREG.
	     Keep them in the following order:
	     all reloads for input reload addresses,
	     all reloads for ordinary input operands,
	     all reloads for addresses of non-reloaded operands,
	     the insn being reloaded,
	     all reloads for addresses of output reloads,
	     the output reloads.  */
	  if (! special)
	    {
#ifdef SECONDARY_INPUT_RELOAD_CLASS
	      rtx second_reload_reg = 0;
	      enum insn_code icode;

	      /* If we have a secondary reload, pick up the secondary register
		 and icode, if any.  If OLDEQUIV and OLD are different or
		 if this is an in-out reload, recompute whether or not we
		 still need a secondary register and what the icode should
		 be.  If we still need a secondary register and the class or
		 icode is different, go back to reloading from OLD if using
		 OLDEQUIV means that we got the wrong type of register.  We
		 cannot have different class or icode due to an in-out reload
		 because we don't make such reloads when both the input and
		 output need secondary reload registers.  */

	      if (reload_secondary_reload[j] >= 0)
		{
		  int secondary_reload = reload_secondary_reload[j];
		  rtx real_oldequiv = oldequiv;
		  rtx real_old = old;

		  /* If OLDEQUIV is a pseudo with a MEM, get the real MEM
		     and similarly for OLD.
		     See comments in find_secondary_reload in reload.c.  */
		  if (GET_CODE (oldequiv) == REG
		      && REGNO (oldequiv) >= FIRST_PSEUDO_REGISTER
		      && reg_equiv_mem[REGNO (oldequiv)] != 0)
		    real_oldequiv = reg_equiv_mem[REGNO (oldequiv)];

		  if (GET_CODE (old) == REG
		      && REGNO (old) >= FIRST_PSEUDO_REGISTER
		      && reg_equiv_mem[REGNO (old)] != 0)
		    real_old = reg_equiv_mem[REGNO (old)];

		  second_reload_reg = reload_reg_rtx[secondary_reload];
		  icode = reload_secondary_icode[j];

		  if ((old != oldequiv && ! rtx_equal_p (old, oldequiv))
		      || (reload_in[j] != 0 && reload_out[j] != 0))
		    {
		      enum reg_class new_class
			= SECONDARY_INPUT_RELOAD_CLASS (reload_reg_class[j],
							mode, real_oldequiv);

		      if (new_class == NO_REGS)
			second_reload_reg = 0;
		      else
			{
			  enum insn_code new_icode;
			  enum machine_mode new_mode;

			  if (! TEST_HARD_REG_BIT (reg_class_contents[(int) new_class],
						   REGNO (second_reload_reg)))
			    oldequiv = old, real_oldequiv = real_old;
			  else
			    {
			      new_icode = reload_in_optab[(int) mode];
			      if (new_icode != CODE_FOR_nothing
				  && ((insn_operand_predicate[(int) new_icode][0]
				       && ! ((*insn_operand_predicate[(int) new_icode][0])
					     (reloadreg, mode)))
				      || (insn_operand_predicate[(int) new_icode][1]
					  && ! ((*insn_operand_predicate[(int) new_icode][1])
						(real_oldequiv, mode)))))
				new_icode = CODE_FOR_nothing;

			      if (new_icode == CODE_FOR_nothing)
				new_mode = mode;
			      else
				new_mode = insn_operand_mode[new_icode][2];

			      if (GET_MODE (second_reload_reg) != new_mode)
				{
				  if (!HARD_REGNO_MODE_OK (REGNO (second_reload_reg),
							   new_mode))
				    oldequiv = old, real_oldequiv = real_old;
				  else
				    second_reload_reg
				      = gen_rtx (REG, new_mode,
						 REGNO (second_reload_reg));
				}
			    }
			}
		    }

		  /* If we still need a secondary reload register, check
		     to see if it is being used as a scratch or intermediate
		     register and generate code appropriately.  If we need
		     a scratch register, use REAL_OLDEQUIV since the form of
		     the insn may depend on the actual address if it is 
		     a MEM.  */

		  if (second_reload_reg)
		    {
		      if (icode != CODE_FOR_nothing)
			{
			  reload_insn = emit_insn_before (GEN_FCN (icode)
							  (reloadreg,
							   real_oldequiv,
							   second_reload_reg),
							  where);
			  if (this_reload_insn == 0)
			    this_reload_insn = reload_insn;
			  special = 1;
			}
		      else
			{
			  /* See if we need a scratch register to load the
			     intermediate register (a tertiary reload).  */
			  enum insn_code tertiary_icode
			    = reload_secondary_icode[secondary_reload];

			  if (tertiary_icode != CODE_FOR_nothing)
			    {
			      rtx third_reload_reg
			        = reload_reg_rtx[reload_secondary_reload[secondary_reload]];

			      reload_insn
				= emit_insn_before ((GEN_FCN (tertiary_icode)
						     (second_reload_reg,
						      real_oldequiv,
						      third_reload_reg)),
						    where);
			      if (this_reload_insn == 0)
				this_reload_insn = reload_insn;
			    }
			  else
			    {
			      reload_insn
				= gen_input_reload (second_reload_reg,
						    oldequiv, where);
			      if (this_reload_insn == 0)
				this_reload_insn = reload_insn;
			      oldequiv = second_reload_reg;
			    }
			}
		    }
		}
#endif

	      if (! special)
		{
		  reload_insn = gen_input_reload (reloadreg, oldequiv, where);
		  if (this_reload_insn == 0)
		    this_reload_insn = reload_insn;
		}

#if defined(SECONDARY_INPUT_RELOAD_CLASS) && defined(PRESERVE_DEATH_INFO_REGNO_P)
	      /* We may have to make a REG_DEAD note for the secondary reload
		 register in the insns we just made.  Find the last insn that
		 mentioned the register.  */
	      if (! special && second_reload_reg
		  && PRESERVE_DEATH_INFO_REGNO_P (REGNO (second_reload_reg)))
		{
		  rtx prev;

		  for (prev = where;
		       prev != PREV_INSN (this_reload_insn);
		       prev = PREV_INSN (prev))
		    if (GET_RTX_CLASS (GET_CODE (prev) == 'i')
			&& reg_overlap_mentioned_for_reload_p (second_reload_reg,
							       PATTERN (prev)))
		      {
			REG_NOTES (prev) = gen_rtx (EXPR_LIST, REG_DEAD,
						    second_reload_reg,
						    REG_NOTES (prev));
			break;
		      }
		}
#endif
	    }

	  /* Update where to put other reload insns.  */
	  if (this_reload_insn)
	    switch (reload_when_needed[j])
	      {
	      case RELOAD_FOR_INPUT:
	      case RELOAD_OTHER:
		if (first_other_reload_insn == first_operand_address_reload_insn)
		  first_other_reload_insn = this_reload_insn;
		break;
	      case RELOAD_FOR_OPERAND_ADDRESS:
		if (first_operand_address_reload_insn == before_insn)
		  first_operand_address_reload_insn = this_reload_insn;
		if (first_other_reload_insn == before_insn)
		  first_other_reload_insn = this_reload_insn;
	      }

	  /* reload_inc[j] was formerly processed here.  */
	}

      /* Add a note saying the input reload reg
	 dies in this insn, if anyone cares.  */
#ifdef PRESERVE_DEATH_INFO_REGNO_P
      if (old != 0
	  && reload_reg_rtx[j] != old
	  && reload_reg_rtx[j] != 0
	  && reload_out[j] == 0
	  && ! reload_inherited[j]
	  && PRESERVE_DEATH_INFO_REGNO_P (REGNO (reload_reg_rtx[j])))
	{
	  register rtx reloadreg = reload_reg_rtx[j];

#if 0
	  /* We can't abort here because we need to support this for sched.c.
	     It's not terrible to miss a REG_DEAD note, but we should try
	     to figure out how to do this correctly.  */
	  /* The code below is incorrect for address-only reloads.  */
	  if (reload_when_needed[j] != RELOAD_OTHER
	      && reload_when_needed[j] != RELOAD_FOR_INPUT)
	    abort ();
#endif

	  /* Add a death note to this insn, for an input reload.  */

	  if ((reload_when_needed[j] == RELOAD_OTHER
	       || reload_when_needed[j] == RELOAD_FOR_INPUT)
	      && ! dead_or_set_p (insn, reloadreg))
	    REG_NOTES (insn)
	      = gen_rtx (EXPR_LIST, REG_DEAD,
			 reloadreg, REG_NOTES (insn));
	}

      /* When we inherit a reload, the last marked death of the reload reg
	 may no longer really be a death.  */
      if (reload_reg_rtx[j] != 0
	  && PRESERVE_DEATH_INFO_REGNO_P (REGNO (reload_reg_rtx[j]))
	  && reload_inherited[j])
	{
	  /* Handle inheriting an output reload.
	     Remove the death note from the output reload insn.  */
	  if (reload_spill_index[j] >= 0
	      && GET_CODE (reload_in[j]) == REG
	      && spill_reg_store[reload_spill_index[j]] != 0
	      && find_regno_note (spill_reg_store[reload_spill_index[j]],
				  REG_DEAD, REGNO (reload_reg_rtx[j])))
	    remove_death (REGNO (reload_reg_rtx[j]),
			  spill_reg_store[reload_spill_index[j]]);
	  /* Likewise for input reloads that were inherited.  */
	  else if (reload_spill_index[j] >= 0
		   && GET_CODE (reload_in[j]) == REG
		   && spill_reg_store[reload_spill_index[j]] == 0
		   && reload_inheritance_insn[j] != 0
		   && find_regno_note (reload_inheritance_insn[j], REG_DEAD,
				       REGNO (reload_reg_rtx[j])))
	    remove_death (REGNO (reload_reg_rtx[j]),
			  reload_inheritance_insn[j]);
	  else
	    {
	      rtx prev;

	      /* We got this register from find_equiv_reg.
		 Search back for its last death note and get rid of it.
		 But don't search back too far.
		 Don't go past a place where this reg is set,
		 since a death note before that remains valid.  */
	      for (prev = PREV_INSN (insn);
		   prev && GET_CODE (prev) != CODE_LABEL;
		   prev = PREV_INSN (prev))
		if (GET_RTX_CLASS (GET_CODE (prev)) == 'i'
		    && dead_or_set_p (prev, reload_reg_rtx[j]))
		  {
		    if (find_regno_note (prev, REG_DEAD,
					 REGNO (reload_reg_rtx[j])))
		      remove_death (REGNO (reload_reg_rtx[j]), prev);
		    break;
		  }
	    }
	}

      /* We might have used find_equiv_reg above to choose an alternate
	 place from which to reload.  If so, and it died, we need to remove
	 that death and move it to one of the insns we just made.  */

      if (oldequiv_reg != 0
	  && PRESERVE_DEATH_INFO_REGNO_P (true_regnum (oldequiv_reg)))
	{
	  rtx prev, prev1;

	  for (prev = PREV_INSN (insn); prev && GET_CODE (prev) != CODE_LABEL;
	       prev = PREV_INSN (prev))
	    if (GET_RTX_CLASS (GET_CODE (prev)) == 'i'
		&& dead_or_set_p (prev, oldequiv_reg))
	      {
		if (find_regno_note (prev, REG_DEAD, REGNO (oldequiv_reg)))
		  {
		    for (prev1 = this_reload_insn;
			 prev1; prev1 = PREV_INSN (prev1))
		      if (GET_RTX_CLASS (GET_CODE (prev1) == 'i')
			&& reg_overlap_mentioned_for_reload_p (oldequiv_reg,
							       PATTERN (prev1)))
		      {
			REG_NOTES (prev1) = gen_rtx (EXPR_LIST, REG_DEAD,
						     oldequiv_reg,
						     REG_NOTES (prev1));
			break;
		      }
		    remove_death (REGNO (oldequiv_reg), prev);
		  }
		break;
	      }
	}
#endif

      /* If we are reloading a register that was recently stored in with an
	 output-reload, see if we can prove there was
	 actually no need to store the old value in it.  */

      if (optimize && reload_inherited[j] && reload_spill_index[j] >= 0
	  /* This is unsafe if some other reload uses the same reg first.  */
	  && (reload_when_needed[j] == RELOAD_OTHER
	      || reload_when_needed[j] == RELOAD_FOR_INPUT
	      || reload_when_needed[j] == RELOAD_FOR_INPUT_RELOAD_ADDRESS)
	  && GET_CODE (reload_in[j]) == REG
#if 0
	  /* There doesn't seem to be any reason to restrict this to pseudos
	     and doing so loses in the case where we are copying from a
	     register of the wrong class.  */
	  && REGNO (reload_in[j]) >= FIRST_PSEUDO_REGISTER
#endif
	  && spill_reg_store[reload_spill_index[j]] != 0
	  && dead_or_set_p (insn, reload_in[j])
	  /* This is unsafe if operand occurs more than once in current
	     insn.  Perhaps some occurrences weren't reloaded.  */
	  && count_occurrences (PATTERN (insn), reload_in[j]) == 1)
	delete_output_reload (insn, j,
			      spill_reg_store[reload_spill_index[j]]);

      /* Input-reloading is done.  Now do output-reloading,
	 storing the value from the reload-register after the main insn
	 if reload_out[j] is nonzero.

	 ??? At some point we need to support handling output reloads of
	 JUMP_INSNs or insns that set cc0.  */
      old = reload_out[j];
      if (old != 0
	  && reload_reg_rtx[j] != old
	  && reload_reg_rtx[j] != 0)
	{
	  register rtx reloadreg = reload_reg_rtx[j];
	  register rtx second_reloadreg = 0;
	  rtx prev_insn = PREV_INSN (first_output_reload_insn);
	  rtx note, p;
	  enum machine_mode mode;
	  int special = 0;

	  /* An output operand that dies right away does need a reload,
	     but need not be copied from it.  Show the new location in the
	     REG_UNUSED note.  */
	  if ((GET_CODE (old) == REG || GET_CODE (old) == SCRATCH)
	      && (note = find_reg_note (insn, REG_UNUSED, old)) != 0)
	    {
	      XEXP (note, 0) = reload_reg_rtx[j];
	      continue;
	    }
	  else if (GET_CODE (old) == SCRATCH)
	    /* If we aren't optimizing, there won't be a REG_UNUSED note,
	       but we don't want to make an output reload.  */
	    continue;

#if 0
	  /* Strip off of OLD any size-increasing SUBREGs such as
	     (SUBREG:SI foo:QI 0).  */

	  while (GET_CODE (old) == SUBREG && SUBREG_WORD (old) == 0
		 && (GET_MODE_SIZE (GET_MODE (old))
		     > GET_MODE_SIZE (GET_MODE (SUBREG_REG (old)))))
	    old = SUBREG_REG (old);
#endif

	  /* If is a JUMP_INSN, we can't support output reloads yet.  */
	  if (GET_CODE (insn) == JUMP_INSN)
	    abort ();

	  /* Determine the mode to reload in.
	     See comments above (for input reloading).  */

	  mode = GET_MODE (old);
	  if (mode == VOIDmode)
	    abort ();		/* Should never happen for an output.  */

	  /* A strict-low-part output operand needs to be reloaded
	     in the mode of the entire value.  */
	  if (reload_strict_low[j])
	    {
	      mode = GET_MODE (SUBREG_REG (reload_out[j]));
	      /* Encapsulate OLD into that mode.  */
	      /* If OLD is a subreg, then strip it, since the subreg will
		 be altered by this very reload.  */
	      while (GET_CODE (old) == SUBREG && GET_MODE (old) != mode)
		old = SUBREG_REG (old);
	      if (GET_MODE (old) != VOIDmode
		  && mode != GET_MODE (old))
		old = gen_rtx (SUBREG, mode, old, 0);
	    }

	  if (GET_MODE (reloadreg) != mode)
	    reloadreg = gen_rtx (REG, mode, REGNO (reloadreg));

#ifdef SECONDARY_OUTPUT_RELOAD_CLASS

	  /* If we need two reload regs, set RELOADREG to the intermediate
	     one, since it will be stored into OUT.  We might need a secondary
	     register only for an input reload, so check again here.  */

	  if (reload_secondary_reload[j] >= 0)
	    {
	      rtx real_old = old;

	      if (GET_CODE (old) == REG && REGNO (old) >= FIRST_PSEUDO_REGISTER
		  && reg_equiv_mem[REGNO (old)] != 0)
		real_old = reg_equiv_mem[REGNO (old)];

	      if((SECONDARY_OUTPUT_RELOAD_CLASS (reload_reg_class[j],
						 mode, real_old)
		  != NO_REGS))
		{
		  second_reloadreg = reloadreg;
		  reloadreg = reload_reg_rtx[reload_secondary_reload[j]];

		  /* See if RELOADREG is to be used as a scratch register
		     or as an intermediate register.  */
		  if (reload_secondary_icode[j] != CODE_FOR_nothing)
		    {
		      emit_insn_before ((GEN_FCN (reload_secondary_icode[j])
					 (real_old, second_reloadreg,
					  reloadreg)),
					first_output_reload_insn);
		      special = 1;
		    }
		  else
		    {
		      /* See if we need both a scratch and intermediate reload
			 register.  */
		      int secondary_reload = reload_secondary_reload[j];
		      enum insn_code tertiary_icode
			= reload_secondary_icode[secondary_reload];
		      rtx pat;

		      if (GET_MODE (reloadreg) != mode)
			reloadreg = gen_rtx (REG, mode, REGNO (reloadreg));

		      if (tertiary_icode != CODE_FOR_nothing)
			{
			  rtx third_reloadreg
			    = reload_reg_rtx[reload_secondary_reload[secondary_reload]];
			  pat = (GEN_FCN (tertiary_icode)
				 (reloadreg, second_reloadreg, third_reloadreg));
			}
#ifdef SECONDARY_MEMORY_NEEDED
		      /* If we need a memory location to do the move, do it that way.  */
		      else if (GET_CODE (reloadreg) == REG
			       && REGNO (reloadreg) < FIRST_PSEUDO_REGISTER
			       && SECONDARY_MEMORY_NEEDED (REGNO_REG_CLASS (REGNO (reloadreg)),
					   REGNO_REG_CLASS (REGNO (second_reloadreg)),
					   GET_MODE (second_reloadreg)))
			{
			  /* Get the memory to use and rewrite both registers
			     to its mode.  */
			  rtx loc = get_secondary_mem (reloadreg,
						       GET_MODE (second_reloadreg));
			  rtx tmp_reloadreg;
			    
			  if (GET_MODE (loc) != GET_MODE (second_reloadreg))
			    second_reloadreg = gen_rtx (REG, GET_MODE (loc),
							REGNO (second_reloadreg));
			  
			  if (GET_MODE (loc) != GET_MODE (reloadreg))
			    tmp_reloadreg = gen_rtx (REG, GET_MODE (loc),
						     REGNO (reloadreg));
			  else
			    tmp_reloadreg = reloadreg;
			  
			  emit_insn_before (gen_move_insn (loc, second_reloadreg),
					    first_output_reload_insn);
			  pat = gen_move_insn (tmp_reloadreg, loc);
			}
#endif
		      else
			pat = gen_move_insn (reloadreg, second_reloadreg);

		      emit_insn_before (pat, first_output_reload_insn);
		    }
		}
	    }
#endif

	  /* Output the last reload insn.  */
	  if (! special)
	    {
#ifdef SECONDARY_MEMORY_NEEDED
	      /* If we need a memory location to do the move, do it that way.  */
	      if (GET_CODE (old) == REG && REGNO (old) < FIRST_PSEUDO_REGISTER
		  && SECONDARY_MEMORY_NEEDED (REGNO_REG_CLASS (REGNO (old)),
					      REGNO_REG_CLASS (REGNO (reloadreg)),
					      GET_MODE (reloadreg)))
		{
		  /* Get the memory to use and rewrite both registers to
		     its mode.  */
		  rtx loc = get_secondary_mem (old, GET_MODE (reloadreg));

		  if (GET_MODE (loc) != GET_MODE (reloadreg))
		    reloadreg = gen_rtx (REG, GET_MODE (loc),
					 REGNO (reloadreg));

		  if (GET_MODE (loc) != GET_MODE (old))
		    old = gen_rtx (REG, GET_MODE (loc), REGNO (old));

		  emit_insn_before (gen_move_insn (loc, reloadreg),
				    first_output_reload_insn);
		  emit_insn_before (gen_move_insn (old, loc),
				    first_output_reload_insn);
		}
	      else
#endif
		emit_insn_before (gen_move_insn (old, reloadreg),
				  first_output_reload_insn);
	    }

#ifdef PRESERVE_DEATH_INFO_REGNO_P
	  /* If final will look at death notes for this reg,
	     put one on the last output-reload insn to use it.  Similarly
	     for any secondary register.  */
	  if (PRESERVE_DEATH_INFO_REGNO_P (REGNO (reloadreg)))
	    for (p = PREV_INSN (first_output_reload_insn);
		 p != prev_insn; p = PREV_INSN (p))
	      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
		  && reg_overlap_mentioned_for_reload_p (reloadreg,
							 PATTERN (p)))
		REG_NOTES (p) = gen_rtx (EXPR_LIST, REG_DEAD,
					 reloadreg, REG_NOTES (p));

#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
	  if (! special
	      && PRESERVE_DEATH_INFO_REGNO_P (REGNO (second_reloadreg)))
	    for (p = PREV_INSN (first_output_reload_insn);
		 p != prev_insn; p = PREV_INSN (p))
	      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
		  && reg_overlap_mentioned_for_reload_p (second_reloadreg,
							 PATTERN (p)))
		REG_NOTES (p) = gen_rtx (EXPR_LIST, REG_DEAD,
					 second_reloadreg, REG_NOTES (p));
#endif
#endif
	  /* Look at all insns we emitted, just to be safe.  */
	  for (p = NEXT_INSN (prev_insn); p != first_output_reload_insn;
	       p = NEXT_INSN (p))
	    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	      {
		/* If this output reload doesn't come from a spill reg,
		   clear any memory of reloaded copies of the pseudo reg.
		   If this output reload comes from a spill reg,
		   reg_has_output_reload will make this do nothing.  */
		note_stores (PATTERN (p), forget_old_reloads_1);

		if (reg_mentioned_p (reload_reg_rtx[j], PATTERN (p)))
		  store_insn = p;
	      }

	  first_output_reload_insn = NEXT_INSN (prev_insn);
	}

      if (reload_spill_index[j] >= 0)
	new_spill_reg_store[reload_spill_index[j]] = store_insn;
    }

  /* Move death notes from INSN
     to output-operand-address and output reload insns.  */
#ifdef PRESERVE_DEATH_INFO_REGNO_P
  {
    rtx insn1;
    /* Loop over those insns, last ones first.  */
    for (insn1 = PREV_INSN (following_insn); insn1 != insn;
	 insn1 = PREV_INSN (insn1))
      if (GET_CODE (insn1) == INSN && GET_CODE (PATTERN (insn1)) == SET)
	{
	  rtx source = SET_SRC (PATTERN (insn1));
	  rtx dest = SET_DEST (PATTERN (insn1));

	  /* The note we will examine next.  */
	  rtx reg_notes = REG_NOTES (insn);
	  /* The place that pointed to this note.  */
	  rtx *prev_reg_note = &REG_NOTES (insn);

	  /* If the note is for something used in the source of this
	     reload insn, or in the output address, move the note.  */
	  while (reg_notes)
	    {
	      rtx next_reg_notes = XEXP (reg_notes, 1);
	      if (REG_NOTE_KIND (reg_notes) == REG_DEAD
		  && GET_CODE (XEXP (reg_notes, 0)) == REG
		  && ((GET_CODE (dest) != REG
		       && reg_overlap_mentioned_for_reload_p (XEXP (reg_notes, 0),
							      dest))
		      || reg_overlap_mentioned_for_reload_p (XEXP (reg_notes, 0),
							     source)))
		{
		  *prev_reg_note = next_reg_notes;
		  XEXP (reg_notes, 1) = REG_NOTES (insn1);
		  REG_NOTES (insn1) = reg_notes;
		}
	      else
		prev_reg_note = &XEXP (reg_notes, 1);

	      reg_notes = next_reg_notes;
	    }
	}
  }
#endif

  /* For all the spill regs newly reloaded in this instruction,
     record what they were reloaded from, so subsequent instructions
     can inherit the reloads.

     Update spill_reg_store for the reloads of this insn.
     Copy the elements that were updated in the loop above.  */

  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];
      register int i = reload_spill_index[r];

      /* I is nonneg if this reload used one of the spill regs.
	 If reload_reg_rtx[r] is 0, this is an optional reload
	 that we opted to ignore.  */

      if (i >= 0 && reload_reg_rtx[r] != 0)
	{
	  /* First, clear out memory of what used to be in this spill reg.
	     If consecutive registers are used, clear them all.  */
	  int nr
	    = HARD_REGNO_NREGS (spill_regs[i], GET_MODE (reload_reg_rtx[r]));
	  int k;

	  for (k = 0; k < nr; k++)
	    {
	      reg_reloaded_contents[spill_reg_order[spill_regs[i] + k]] = -1;
	      reg_reloaded_insn[spill_reg_order[spill_regs[i] + k]] = 0;
	    }

	  /* Maybe the spill reg contains a copy of reload_out.  */
	  if (reload_out[r] != 0 && GET_CODE (reload_out[r]) == REG)
	    {
	      register int nregno = REGNO (reload_out[r]);

	      spill_reg_store[i] = new_spill_reg_store[i];
	      reg_last_reload_reg[nregno] = reload_reg_rtx[r];

	      for (k = 0; k < nr; k++)
		{
		  reg_reloaded_contents[spill_reg_order[spill_regs[i] + k]]
		    = nregno;
		  reg_reloaded_insn[spill_reg_order[spill_regs[i] + k]] = insn;
		}
	    }

	  /* Maybe the spill reg contains a copy of reload_in.  */
	  else if (reload_out[r] == 0
		   && reload_in[r] != 0
		   && (GET_CODE (reload_in[r]) == REG
		       || GET_CODE (reload_in_reg[r]) == REG))
	    {
	      register int nregno;
	      if (GET_CODE (reload_in[r]) == REG)
		nregno = REGNO (reload_in[r]);
	      else
		nregno = REGNO (reload_in_reg[r]);

	      /* If there are two separate reloads (one in and one out)
		 for the same (hard or pseudo) reg,
		 leave reg_last_reload_reg set
		 based on the output reload.
		 Otherwise, set it from this input reload.  */
	      if (!reg_has_output_reload[nregno]
		  /* But don't do so if another input reload
		     will clobber this one's value.  */
		  && reload_reg_reaches_end_p (spill_regs[i],
					       reload_when_needed[r]))
		{
		  reg_last_reload_reg[nregno] = reload_reg_rtx[r];

		  /* Unless we inherited this reload, show we haven't
		     recently done a store.  */
		  if (! reload_inherited[r])
		    spill_reg_store[i] = 0;

		  for (k = 0; k < nr; k++)
		    {
		      reg_reloaded_contents[spill_reg_order[spill_regs[i] + k]]
			= nregno;
		      reg_reloaded_insn[spill_reg_order[spill_regs[i] + k]]
			= insn;
		    }
		}
	    }
	}

      /* The following if-statement was #if 0'd in 1.34 (or before...).
	 It's reenabled in 1.35 because supposedly nothing else
	 deals with this problem.  */

      /* If a register gets output-reloaded from a non-spill register,
	 that invalidates any previous reloaded copy of it.
	 But forget_old_reloads_1 won't get to see it, because
	 it thinks only about the original insn.  So invalidate it here.  */
      if (i < 0 && reload_out[r] != 0 && GET_CODE (reload_out[r]) == REG)
	{
	  register int nregno = REGNO (reload_out[r]);
	  reg_last_reload_reg[nregno] = 0;
	}
    }
}

/* Emit code before BEFORE_INSN to perform an input reload of IN to RELOADREG.
   Returns first insn emitted.  */

rtx
gen_input_reload (reloadreg, in, before_insn)
     rtx reloadreg;
     rtx in;
     rtx before_insn;
{
  register rtx prev_insn = PREV_INSN (before_insn);

  /* How to do this reload can get quite tricky.  Normally, we are being
     asked to reload a simple operand, such as a MEM, a constant, or a pseudo
     register that didn't get a hard register.  In that case we can just
     call emit_move_insn.

     We can also be asked to reload a PLUS that adds either two registers or
     a register and a constant or MEM.  This can occur during frame pointer
     elimination.  That case if handled by trying to emit a single insn
     to perform the add.  If it is not valid, we use a two insn sequence.

     Finally, we could be called to handle an 'o' constraint by putting
     an address into a register.  In that case, we first try to do this
     with a named pattern of "reload_load_address".  If no such pattern
     exists, we just emit a SET insn and hope for the best (it will normally
     be valid on machines that use 'o').

     This entire process is made complex because reload will never
     process the insns we generate here and so we must ensure that
     they will fit their constraints and also by the fact that parts of
     IN might be being reloaded separately and replaced with spill registers.
     Because of this, we are, in some sense, just guessing the right approach
     here.  The one listed above seems to work.

     ??? At some point, this whole thing needs to be rethought.  */

  if (GET_CODE (in) == PLUS
      && GET_CODE (XEXP (in, 0)) == REG
      && (GET_CODE (XEXP (in, 1)) == REG
	  || CONSTANT_P (XEXP (in, 1))
	  || GET_CODE (XEXP (in, 1)) == MEM))
    {
      /* We need to compute the sum of what is either a register and a
	 constant, a register and memory, or a hard register and a pseudo
	 register and put it into the reload register.  The best possible way
	 of doing this is if the machine has a three-operand ADD insn that
	 accepts the required operands.

	 The simplest approach is to try to generate such an insn and see if it
	 is recognized and matches its constraints.  If so, it can be used.

	 It might be better not to actually emit the insn unless it is valid,
	 but we need to pass the insn as an operand to `recog' and
	 `insn_extract' and it is simpler to emit and then delete the insn if
	 not valid than to dummy things up.  */

      rtx op0, op1, tem, insn;
      int code;

      op0 = find_replacement (&XEXP (in, 0));
      op1 = find_replacement (&XEXP (in, 1));

      /* Since constraint checking is strict, commutativity won't be
	 checked, so we need to do that here to avoid spurious failure
	 if the add instruction is two-address and the second operand
	 of the add is the same as the reload reg, which is frequently
	 the case.  If the insn would be A = B + A, rearrange it so
	 it will be A = A + B as constrain_operands expects. */

      if (GET_CODE (XEXP (in, 1)) == REG
	  && REGNO (reloadreg) == REGNO (XEXP (in, 1)))
	tem = op0, op0 = op1, op1 = tem;

      if (op0 != XEXP (in, 0) || op1 != XEXP (in, 1))
	in = gen_rtx (PLUS, GET_MODE (in), op0, op1);

      insn = emit_insn_before (gen_rtx (SET, VOIDmode, reloadreg, in),
				   before_insn);
      code = recog_memoized (insn);

      if (code >= 0)
	{
	  insn_extract (insn);
	  /* We want constrain operands to treat this insn strictly in
	     its validity determination, i.e., the way it would after reload
	     has completed.  */
	  if (constrain_operands (code, 1))
	    return insn;
	}

      if (PREV_INSN (insn))
	NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
      if (NEXT_INSN (insn))
	PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);

      /* If that failed, we must use a conservative two-insn sequence.
	 use move to copy constant, MEM, or pseudo register to the reload
	 register since "move" will be able to handle an arbitrary operand,
	 unlike add which can't, in general.  Then add the registers.

	 If there is another way to do this for a specific machine, a
	 DEFINE_PEEPHOLE should be specified that recognizes the sequence
	 we emit below.  */

      if (CONSTANT_P (op1) || GET_CODE (op1) == MEM
	  || (GET_CODE (op1) == REG
	      && REGNO (op1) >= FIRST_PSEUDO_REGISTER))
	tem = op0, op0 = op1, op1 = tem;

      emit_insn_before (gen_move_insn (reloadreg, op0), before_insn);

      /* If OP0 and OP1 are the same, we can use RELOADREG for OP1.
	 This fixes a problem on the 32K where the stack pointer cannot
	 be used as an operand of an add insn.  */

      if (rtx_equal_p (op0, op1))
	op1 = reloadreg;

      emit_insn_before (gen_add2_insn (reloadreg, op1), before_insn);
    }

#ifdef SECONDARY_MEMORY_NEEDED
  /* If we need a memory location to do the move, do it that way.  */
  else if (GET_CODE (in) == REG && REGNO (in) < FIRST_PSEUDO_REGISTER
	   && SECONDARY_MEMORY_NEEDED (REGNO_REG_CLASS (REGNO (in)),
				       REGNO_REG_CLASS (REGNO (reloadreg)),
				       GET_MODE (reloadreg)))
    {
      /* Get the memory to use and rewrite both registers to its mode.  */
      rtx loc = get_secondary_mem (in, GET_MODE (reloadreg));

      if (GET_MODE (loc) != GET_MODE (reloadreg))
	reloadreg = gen_rtx (REG, GET_MODE (loc), REGNO (reloadreg));

      if (GET_MODE (loc) != GET_MODE (in))
	in = gen_rtx (REG, GET_MODE (loc), REGNO (in));

      emit_insn_before (gen_move_insn (loc, in), before_insn);
      emit_insn_before (gen_move_insn (reloadreg, loc), before_insn);
    }
#endif

  /* If IN is a simple operand, use gen_move_insn.  */
  else if (GET_RTX_CLASS (GET_CODE (in)) == 'o' || GET_CODE (in) == SUBREG)
    emit_insn_before (gen_move_insn (reloadreg, in), before_insn);

#ifdef HAVE_reload_load_address
  else if (HAVE_reload_load_address)
    emit_insn_before (gen_reload_load_address (reloadreg, in), before_insn);
#endif

  /* Otherwise, just write (set REGLOADREG IN) and hope for the best.  */
  else
    emit_insn_before (gen_rtx (SET, VOIDmode, reloadreg, in), before_insn);

  /* Return the first insn emitted.
     We can not just return PREV_INSN (before_insn), because there may have
     been multiple instructions emitted.  Also note that gen_move_insn may
     emit more than one insn itself, so we can not assume that there is one
     insn emitted per emit_insn_before call.  */

  return NEXT_INSN (prev_insn);
}

/* Delete a previously made output-reload
   whose result we now believe is not needed.
   First we double-check.

   INSN is the insn now being processed.
   OUTPUT_RELOAD_INSN is the insn of the output reload.
   J is the reload-number for this insn.  */

static void
delete_output_reload (insn, j, output_reload_insn)
     rtx insn;
     int j;
     rtx output_reload_insn;
{
  register rtx i1;

  /* Get the raw pseudo-register referred to.  */

  rtx reg = reload_in[j];
  while (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  /* If the pseudo-reg we are reloading is no longer referenced
     anywhere between the store into it and here,
     and no jumps or labels intervene, then the value can get
     here through the reload reg alone.
     Otherwise, give up--return.  */
  for (i1 = NEXT_INSN (output_reload_insn);
       i1 != insn; i1 = NEXT_INSN (i1))
    {
      if (GET_CODE (i1) == CODE_LABEL || GET_CODE (i1) == JUMP_INSN)
	return;
      if ((GET_CODE (i1) == INSN || GET_CODE (i1) == CALL_INSN)
	  && reg_mentioned_p (reg, PATTERN (i1)))
	return;
    }

  /* If this insn will store in the pseudo again,
     the previous store can be removed.  */
  if (reload_out[j] == reload_in[j])
    delete_insn (output_reload_insn);

  /* See if the pseudo reg has been completely replaced
     with reload regs.  If so, delete the store insn
     and forget we had a stack slot for the pseudo.  */
  else if (reg_n_deaths[REGNO (reg)] == 1
	   && reg_basic_block[REGNO (reg)] >= 0
	   && find_regno_note (insn, REG_DEAD, REGNO (reg)))
    {
      rtx i2;

      /* We know that it was used only between here
	 and the beginning of the current basic block.
	 (We also know that the last use before INSN was
	 the output reload we are thinking of deleting, but never mind that.)
	 Search that range; see if any ref remains.  */
      for (i2 = PREV_INSN (insn); i2; i2 = PREV_INSN (i2))
	{
	  rtx set = single_set (i2);

	  /* Uses which just store in the pseudo don't count,
	     since if they are the only uses, they are dead.  */
	  if (set != 0 && SET_DEST (set) == reg)
	    continue;
	  if (GET_CODE (i2) == CODE_LABEL
	      || GET_CODE (i2) == JUMP_INSN)
	    break;
	  if ((GET_CODE (i2) == INSN || GET_CODE (i2) == CALL_INSN)
	      && reg_mentioned_p (reg, PATTERN (i2)))
	    /* Some other ref remains;
	       we can't do anything.  */
	    return;
	}

      /* Delete the now-dead stores into this pseudo.  */
      for (i2 = PREV_INSN (insn); i2; i2 = PREV_INSN (i2))
	{
	  rtx set = single_set (i2);

	  if (set != 0 && SET_DEST (set) == reg)
	    delete_insn (i2);
	  if (GET_CODE (i2) == CODE_LABEL
	      || GET_CODE (i2) == JUMP_INSN)
	    break;
	}

      /* For the debugging info,
	 say the pseudo lives in this reload reg.  */
      reg_renumber[REGNO (reg)] = REGNO (reload_reg_rtx[j]);
      alter_reg (REGNO (reg), -1);
    }
}


/* Output reload-insns to reload VALUE into RELOADREG.
   VALUE is an autoincrement or autodecrement RTX whose operand
   is a register or memory location;
   so reloading involves incrementing that location.

   INC_AMOUNT is the number to increment or decrement by (always positive).
   This cannot be deduced from VALUE.

   INSN is the insn before which the new insns should be emitted.

   The return value is the first of the insns emitted.  */

static rtx
inc_for_reload (reloadreg, value, inc_amount, insn)
     rtx reloadreg;
     rtx value;
     int inc_amount;
     rtx insn;
{
  /* REG or MEM to be copied and incremented.  */
  rtx incloc = XEXP (value, 0);
  /* Nonzero if increment after copying.  */
  int post = (GET_CODE (value) == POST_DEC || GET_CODE (value) == POST_INC);
  rtx prev = PREV_INSN (insn);
  rtx inc;
  rtx add_insn;
  int code;

  /* No hard register is equivalent to this register after
     inc/dec operation.  If REG_LAST_RELOAD_REG were non-zero,
     we could inc/dec that register as well (maybe even using it for
     the source), but I'm not sure it's worth worrying about.  */
  if (GET_CODE (incloc) == REG)
    reg_last_reload_reg[REGNO (incloc)] = 0;

  if (GET_CODE (value) == PRE_DEC || GET_CODE (value) == POST_DEC)
    inc_amount = - inc_amount;

  inc = GEN_INT (inc_amount);

  /* If this is post-increment, first copy the location to the reload reg.  */
  if (post)
    emit_insn_before (gen_move_insn (reloadreg, incloc), insn);

  /* See if we can directly increment INCLOC.  Use a method similar to that
     in gen_input_reload.  */

  add_insn = emit_insn_before (gen_rtx (SET, VOIDmode, incloc,
					gen_rtx (PLUS, GET_MODE (incloc),
						 incloc, inc)), insn);
							  
  code = recog_memoized (add_insn);
  if (code >= 0)
    {
      insn_extract (add_insn);
      if (constrain_operands (code, 1))
	{
	  /* If this is a pre-increment and we have incremented the value
	     where it lives, copy the incremented value to RELOADREG to
	     be used as an address.  */

	  if (! post)
	    emit_insn_before (gen_move_insn (reloadreg, incloc), insn);
	  return NEXT_INSN (prev);
	}
    }

  if (PREV_INSN (add_insn))
    NEXT_INSN (PREV_INSN (add_insn)) = NEXT_INSN (add_insn);
  if (NEXT_INSN (add_insn))
    PREV_INSN (NEXT_INSN (add_insn)) = PREV_INSN (add_insn);

  /* If couldn't do the increment directly, must increment in RELOADREG.
     The way we do this depends on whether this is pre- or post-increment.
     For pre-increment, copy INCLOC to the reload register, increment it
     there, then save back.  */

  if (! post)
    {
      emit_insn_before (gen_move_insn (reloadreg, incloc), insn);
      emit_insn_before (gen_add2_insn (reloadreg, inc), insn);
      emit_insn_before (gen_move_insn (incloc, reloadreg), insn);
    }
  else
    {
      /* Postincrement.
	 Because this might be a jump insn or a compare, and because RELOADREG
	 may not be available after the insn in an input reload, we must do
	 the incrementation before the insn being reloaded for.

	 We have already copied INCLOC to RELOADREG.  Increment the copy in
	 RELOADREG, save that back, then decrement RELOADREG so it has
	 the original value.  */

      emit_insn_before (gen_add2_insn (reloadreg, inc), insn);
      emit_insn_before (gen_move_insn (incloc, reloadreg), insn);
      emit_insn_before (gen_add2_insn (reloadreg, GEN_INT (-inc_amount)),
			insn);
    }

  return NEXT_INSN (prev);
}

/* Return 1 if we are certain that the constraint-string STRING allows
   the hard register REG.  Return 0 if we can't be sure of this.  */

static int
constraint_accepts_reg_p (string, reg)
     char *string;
     rtx reg;
{
  int value = 0;
  int regno = true_regnum (reg);
  int c;

  /* Initialize for first alternative.  */
  value = 0;
  /* Check that each alternative contains `g' or `r'.  */
  while (1)
    switch (c = *string++)
      {
      case 0:
	/* If an alternative lacks `g' or `r', we lose.  */
	return value;
      case ',':
	/* If an alternative lacks `g' or `r', we lose.  */
	if (value == 0)
	  return 0;
	/* Initialize for next alternative.  */
	value = 0;
	break;
      case 'g':
      case 'r':
	/* Any general reg wins for this alternative.  */
	if (TEST_HARD_REG_BIT (reg_class_contents[(int) GENERAL_REGS], regno))
	  value = 1;
	break;
      default:
	/* Any reg in specified class wins for this alternative.  */
	{
	  enum reg_class class = REG_CLASS_FROM_LETTER (c);

	  if (TEST_HARD_REG_BIT (reg_class_contents[(int) class], regno))
	    value = 1;
	}
      }
}

/* Return the number of places FIND appears within X, but don't count
   an occurrence if some SET_DEST is FIND.  */

static int
count_occurrences (x, find)
     register rtx x, find;
{
  register int i, j;
  register enum rtx_code code;
  register char *format_ptr;
  int count;

  if (x == find)
    return 1;
  if (x == 0)
    return 0;

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return 0;

    case SET:
      if (SET_DEST (x) == find)
	return count_occurrences (SET_SRC (x), find);
      break;
    }

  format_ptr = GET_RTX_FORMAT (code);
  count = 0;

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  count += count_occurrences (XEXP (x, i), find);
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      for (j = 0; j < XVECLEN (x, i); j++)
		count += count_occurrences (XVECEXP (x, i, j), find);
	    }
	  break;
	}
    }
  return count;
}
