/* LRA (local register allocator) driver and LRA utilities.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
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
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */


/* The Local Register Allocator (LRA) is a replacement of former
   reload pass.	 It is focused to simplify code solving the reload
   pass tasks, to make the code maintenance easier, and to implement new
   perspective optimizations.

   The major LRA design solutions are:
     o division small manageable, separated sub-tasks
     o reflection of all transformations and decisions in RTL as more
       as possible
     o insn constraints as a primary source of the info (minimizing
       number of target-depended macros/hooks)

   In brief LRA works by iterative insn process with the final goal is
   to satisfy all insn and address constraints:
     o New reload insns (in brief reloads) and reload pseudos might be
       generated;
     o Some pseudos might be spilled to assign hard registers to
       new reload pseudos;
     o Recalculating spilled pseudo values (rematerialization);
     o Changing spilled pseudos to stack memory or their equivalences;
     o Allocation stack memory changes the address displacement and
       new iteration is needed.

   Here is block diagram of LRA passes:

                                ------------------------
           ---------------     | Undo inheritance for   |     ---------------
          | Memory-memory |    | spilled pseudos,       |    | New (and old) |
          | move coalesce |<---| splits for pseudos got |<-- |   pseudos     |
           ---------------     | the same hard regs,    |    |  assignment   |
  Start           |            | and optional reloads   |     ---------------
    |             |             ------------------------            ^
    V             |              ----------------                   |
 -----------      V             | Update virtual |                  |
|  Remove   |----> ------------>|    register    |                  |
| scratches |     ^             |  displacements |                  |
 -----------      |              ----------------                   |
                  |                      |                          |
                  |                      V         New              |
                  |                 ------------  pseudos   -------------------
                  |                |Constraints:| or insns | Inheritance/split |
                  |                |    RTL     |--------->|  transformations  |
                  |                | transfor-  |          |    in EBB scope   |
                  | substi-        |  mations   |           -------------------
                  | tutions         ------------
                  |                     | No change
          ----------------              V
         | Spilled pseudo |      -------------------
         |    to memory   |<----| Rematerialization |
         |  substitution  |      -------------------
          ----------------        
                  | No susbtitions
                  V                
      -------------------------
     | Hard regs substitution, |
     |  devirtalization, and   |------> Finish
     | restoring scratches got |
     |         memory          |
      -------------------------

   To speed up the process:
     o We process only insns affected by changes on previous
       iterations;
     o We don't use DFA-infrastructure because it results in much slower
       compiler speed than a special IR described below does;
     o We use a special insn representation for quick access to insn
       info which is always *synchronized* with the current RTL;
       o Insn IR is minimized by memory.  It is divided on three parts:
	 o one specific for each insn in RTL (only operand locations);
	 o one common for all insns in RTL with the same insn code
	   (different operand attributes from machine descriptions);
	 o one oriented for maintenance of live info (list of pseudos).
       o Pseudo data:
	 o all insns where the pseudo is referenced;
	 o live info (conflicting hard regs, live ranges, # of
	   references etc);
	 o data used for assigning (preferred hard regs, costs etc).

   This file contains LRA driver, LRA utility functions and data, and
   code for dealing with scratches.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "expr.h"
#include "cfgrtl.h"
#include "cfgbuild.h"
#include "lra.h"
#include "lra-int.h"
#include "print-rtl.h"

/* Dump bitmap SET with TITLE and BB INDEX.  */
void
lra_dump_bitmap_with_title (const char *title, bitmap set, int index)
{
  unsigned int i;
  int count;
  bitmap_iterator bi;
  static const int max_nums_on_line = 10;

  if (bitmap_empty_p (set))
    return;
  fprintf (lra_dump_file, "  %s %d:", title, index);
  fprintf (lra_dump_file, "\n");
  count = max_nums_on_line + 1;
  EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
    {
      if (count > max_nums_on_line)
	{
	  fprintf (lra_dump_file, "\n    ");
	  count = 0;
	}
      fprintf (lra_dump_file, " %4u", i);
      count++;
    }
  fprintf (lra_dump_file, "\n");
}

/* Hard registers currently not available for allocation.  It can
   changed after some hard  registers become not eliminable.  */
HARD_REG_SET lra_no_alloc_regs;

static int get_new_reg_value (void);
static void expand_reg_info (void);
static void invalidate_insn_recog_data (int);
static int get_insn_freq (rtx_insn *);
static void invalidate_insn_data_regno_info (lra_insn_recog_data_t,
					     rtx_insn *, int);

/* Expand all regno related info needed for LRA.  */
static void
expand_reg_data (int old)
{
  resize_reg_info ();
  expand_reg_info ();
  ira_expand_reg_equiv ();
  for (int i = (int) max_reg_num () - 1; i >= old; i--)
    lra_change_class (i, ALL_REGS, "      Set", true);
}

/* Create and return a new reg of ORIGINAL mode.  If ORIGINAL is NULL
   or of VOIDmode, use MD_MODE for the new reg.  Initialize its
   register class to RCLASS.  Print message about assigning class
   RCLASS containing new register name TITLE unless it is NULL.  Use
   attributes of ORIGINAL if it is a register.  The created register
   will have unique held value.  */
rtx
lra_create_new_reg_with_unique_value (machine_mode md_mode, rtx original,
				      enum reg_class rclass, const char *title)
{
  machine_mode mode;
  rtx new_reg;

  if (original == NULL_RTX || (mode = GET_MODE (original)) == VOIDmode)
    mode = md_mode;
  lra_assert (mode != VOIDmode);
  new_reg = gen_reg_rtx (mode);
  if (original == NULL_RTX || ! REG_P (original))
    {
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "      Creating newreg=%i", REGNO (new_reg));
    }
  else
    {
      if (ORIGINAL_REGNO (original) >= FIRST_PSEUDO_REGISTER)
	ORIGINAL_REGNO (new_reg) = ORIGINAL_REGNO (original);
      REG_USERVAR_P (new_reg) = REG_USERVAR_P (original);
      REG_POINTER (new_reg) = REG_POINTER (original);
      REG_ATTRS (new_reg) = REG_ATTRS (original);
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "      Creating newreg=%i from oldreg=%i",
		 REGNO (new_reg), REGNO (original));
    }
  if (lra_dump_file != NULL)
    {
      if (title != NULL)
	fprintf (lra_dump_file, ", assigning class %s to%s%s r%d",
		 reg_class_names[rclass], *title == '\0' ? "" : " ",
		 title, REGNO (new_reg));
      fprintf (lra_dump_file, "\n");
    }
  expand_reg_data (max_reg_num ());
  setup_reg_classes (REGNO (new_reg), rclass, NO_REGS, rclass);
  return new_reg;
}

/* Analogous to the previous function but also inherits value of
   ORIGINAL.  */
rtx
lra_create_new_reg (machine_mode md_mode, rtx original,
		    enum reg_class rclass, const char *title)
{
  rtx new_reg;

  new_reg
    = lra_create_new_reg_with_unique_value (md_mode, original, rclass, title);
  if (original != NULL_RTX && REG_P (original))
    lra_assign_reg_val (REGNO (original), REGNO (new_reg));
  return new_reg;
}

/* Set up for REGNO unique hold value.	*/
void
lra_set_regno_unique_value (int regno)
{
  lra_reg_info[regno].val = get_new_reg_value ();
}

/* Invalidate INSN related info used by LRA.  The info should never be
   used after that.  */
void
lra_invalidate_insn_data (rtx_insn *insn)
{
  lra_invalidate_insn_regno_info (insn);
  invalidate_insn_recog_data (INSN_UID (insn));
}

/* Mark INSN deleted and invalidate the insn related info used by
   LRA.	 */
void
lra_set_insn_deleted (rtx_insn *insn)
{
  lra_invalidate_insn_data (insn);
  SET_INSN_DELETED (insn);
}

/* Delete an unneeded INSN and any previous insns who sole purpose is
   loading data that is dead in INSN.  */
void
lra_delete_dead_insn (rtx_insn *insn)
{
  rtx_insn *prev = prev_real_insn (insn);
  rtx prev_dest;

  /* If the previous insn sets a register that dies in our insn,
     delete it too.  */
  if (prev && GET_CODE (PATTERN (prev)) == SET
      && (prev_dest = SET_DEST (PATTERN (prev)), REG_P (prev_dest))
      && reg_mentioned_p (prev_dest, PATTERN (insn))
      && find_regno_note (insn, REG_DEAD, REGNO (prev_dest))
      && ! side_effects_p (SET_SRC (PATTERN (prev))))
    lra_delete_dead_insn (prev);

  lra_set_insn_deleted (insn);
}

/* Emit insn x = y + z.  Return NULL if we failed to do it.
   Otherwise, return the insn.  We don't use gen_add3_insn as it might
   clobber CC.  */
static rtx_insn *
emit_add3_insn (rtx x, rtx y, rtx z)
{
  rtx_insn *last;

  last = get_last_insn ();

  if (have_addptr3_insn (x, y, z))
    {
      rtx_insn *insn = gen_addptr3_insn (x, y, z);

      /* If the target provides an "addptr" pattern it hopefully does
	 for a reason.  So falling back to the normal add would be
	 a bug.  */
      lra_assert (insn != NULL_RTX);
      emit_insn (insn);
      return insn;
    }

  rtx_insn *insn = emit_insn (gen_rtx_SET (x, gen_rtx_PLUS (GET_MODE (y),
							    y, z)));
  if (recog_memoized (insn) < 0)
    {
      delete_insns_since (last);
      insn = NULL;
    }
  return insn;
}

/* Emit insn x = x + y.  Return the insn.  We use gen_add2_insn as the
   last resort.  */
static rtx_insn *
emit_add2_insn (rtx x, rtx y)
{
  rtx_insn *insn = emit_add3_insn (x, x, y);
  if (insn == NULL_RTX)
    {
      insn = gen_add2_insn (x, y);
      if (insn != NULL_RTX)
	emit_insn (insn);
    }
  return insn;
}

/* Target checks operands through operand predicates to recognize an
   insn.  We should have a special precaution to generate add insns
   which are frequent results of elimination.

   Emit insns for x = y + z.  X can be used to store intermediate
   values and should be not in Y and Z when we use X to store an
   intermediate value.  Y + Z should form [base] [+ index[ * scale]] [
   + disp] where base and index are registers, disp and scale are
   constants.  Y should contain base if it is present, Z should
   contain disp if any.  index[*scale] can be part of Y or Z.  */
void
lra_emit_add (rtx x, rtx y, rtx z)
{
  int old;
  rtx_insn *last;
  rtx a1, a2, base, index, disp, scale, index_scale;
  bool ok_p;

  rtx_insn *add3_insn = emit_add3_insn (x, y, z);
  old = max_reg_num ();
  if (add3_insn != NULL)
    ;
  else
    {
      disp = a2 = NULL_RTX;
      if (GET_CODE (y) == PLUS)
	{
	  a1 = XEXP (y, 0);
	  a2 = XEXP (y, 1);
	  disp = z;
	}
      else
	{
	  a1 = y;
	  if (CONSTANT_P (z))
	    disp = z;
	  else
	    a2 = z;
	}
      index_scale = scale = NULL_RTX;
      if (GET_CODE (a1) == MULT)
	{
	  index_scale = a1;
	  index = XEXP (a1, 0);
	  scale = XEXP (a1, 1);
	  base = a2;
	}
      else if (a2 != NULL_RTX && GET_CODE (a2) == MULT)
	{
	  index_scale = a2;
	  index = XEXP (a2, 0);
	  scale = XEXP (a2, 1);
	  base = a1;
	}
      else
	{
	  base = a1;
	  index = a2;
	}
      if ((base != NULL_RTX && ! (REG_P (base) || GET_CODE (base) == SUBREG))
	  || (index != NULL_RTX
	      && ! (REG_P (index) || GET_CODE (index) == SUBREG))
	  || (disp != NULL_RTX && ! CONSTANT_P (disp))
	  || (scale != NULL_RTX && ! CONSTANT_P (scale)))
	{
	  /* Probably we have no 3 op add.  Last chance is to use 2-op
	     add insn.  To succeed, don't move Z to X as an address
	     segment always comes in Y.  Otherwise, we might fail when
	     adding the address segment to register.  */
	  lra_assert (x != y && x != z);
	  emit_move_insn (x, y);
	  rtx_insn *insn = emit_add2_insn (x, z);
	  lra_assert (insn != NULL_RTX);
	}
      else
	{
	  if (index_scale == NULL_RTX)
	    index_scale = index;
	  if (disp == NULL_RTX)
	    {
	      /* Generate x = index_scale; x = x + base.  */
	      lra_assert (index_scale != NULL_RTX && base != NULL_RTX);
	      emit_move_insn (x, index_scale);
	      rtx_insn *insn = emit_add2_insn (x, base);
	      lra_assert (insn != NULL_RTX);
	    }
	  else if (scale == NULL_RTX)
	    {
	      /* Try x = base + disp.  */
	      lra_assert (base != NULL_RTX);
	      last = get_last_insn ();
	      rtx_insn *move_insn =
		emit_move_insn (x, gen_rtx_PLUS (GET_MODE (base), base, disp));
	      if (recog_memoized (move_insn) < 0)
		{
		  delete_insns_since (last);
		  /* Generate x = disp; x = x + base.  */
		  emit_move_insn (x, disp);
		  rtx_insn *add2_insn = emit_add2_insn (x, base);
		  lra_assert (add2_insn != NULL_RTX);
		}
	      /* Generate x = x + index.  */
	      if (index != NULL_RTX)
		{
		  rtx_insn *insn = emit_add2_insn (x, index);
		  lra_assert (insn != NULL_RTX);
		}
	    }
	  else
	    {
	      /* Try x = index_scale; x = x + disp; x = x + base.  */
	      last = get_last_insn ();
	      rtx_insn *move_insn = emit_move_insn (x, index_scale);
	      ok_p = false;
	      if (recog_memoized (move_insn) >= 0)
		{
		  rtx_insn *insn = emit_add2_insn (x, disp);
		  if (insn != NULL_RTX)
		    {
		      if (base == NULL_RTX)
			ok_p = true;
		      else
			{
			  insn = emit_add2_insn (x, base);
			  if (insn != NULL_RTX)
			    ok_p = true;
			}
		    }
		}
	      if (! ok_p)
		{
		  rtx_insn *insn;
		  
		  delete_insns_since (last);
		  /* Generate x = disp; x = x + base; x = x + index_scale.  */
		  emit_move_insn (x, disp);
		  if (base != NULL_RTX)
		    {
		      insn = emit_add2_insn (x, base);
		      lra_assert (insn != NULL_RTX);
		    }
		  insn = emit_add2_insn (x, index_scale);
		  lra_assert (insn != NULL_RTX);
		}
	    }
	}
    }
  /* Functions emit_... can create pseudos -- so expand the pseudo
     data.  */
  if (old != max_reg_num ())
    expand_reg_data (old);
}

/* The number of emitted reload insns so far.  */
int lra_curr_reload_num;

/* Emit x := y, processing special case when y = u + v or y = u + v *
   scale + w through emit_add (Y can be an address which is base +
   index reg * scale + displacement in general case).  X may be used
   as intermediate result therefore it should be not in Y.  */
void
lra_emit_move (rtx x, rtx y)
{
  int old;

  if (GET_CODE (y) != PLUS)
    {
      if (rtx_equal_p (x, y))
	return;
      old = max_reg_num ();
      emit_move_insn (x, y);
      if (REG_P (x))
	lra_reg_info[ORIGINAL_REGNO (x)].last_reload = ++lra_curr_reload_num;
      /* Function emit_move can create pseudos -- so expand the pseudo
	 data.	*/
      if (old != max_reg_num ())
	expand_reg_data (old);
      return;
    }
  lra_emit_add (x, XEXP (y, 0), XEXP (y, 1));
}

/* Update insn operands which are duplication of operands whose
   numbers are in array of NOPS (with end marker -1).  The insn is
   represented by its LRA internal representation ID.  */
void
lra_update_dups (lra_insn_recog_data_t id, signed char *nops)
{
  int i, j, nop;
  struct lra_static_insn_data *static_id = id->insn_static_data;

  for (i = 0; i < static_id->n_dups; i++)
    for (j = 0; (nop = nops[j]) >= 0; j++)
      if (static_id->dup_num[i] == nop)
	*id->dup_loc[i] = *id->operand_loc[nop];
}



/* This page contains code dealing with info about registers in the
   insns.  */

/* Pools for insn reg info.  */
object_allocator<lra_insn_reg> lra_insn_reg_pool ("insn regs");

/* Create LRA insn related info about a reference to REGNO in INSN
   with TYPE (in/out/inout), biggest reference mode MODE, flag that it
   is reference through subreg (SUBREG_P), flag that is early
   clobbered in the insn (EARLY_CLOBBER), and reference to the next
   insn reg info (NEXT).  If REGNO can be early clobbered,
   alternatives in which it can be early clobbered are given by
   EARLY_CLOBBER_ALTS.  */
static struct lra_insn_reg *
new_insn_reg (rtx_insn *insn, int regno, enum op_type type,
	      machine_mode mode,
	      bool subreg_p, bool early_clobber,
	      alternative_mask early_clobber_alts,
	      struct lra_insn_reg *next)
{
  lra_insn_reg *ir = lra_insn_reg_pool.allocate ();
  ir->type = type;
  ir->biggest_mode = mode;
  if (NONDEBUG_INSN_P (insn)
      && partial_subreg_p (lra_reg_info[regno].biggest_mode, mode))
    lra_reg_info[regno].biggest_mode = mode;
  ir->subreg_p = subreg_p;
  ir->early_clobber = early_clobber;
  ir->early_clobber_alts = early_clobber_alts;
  ir->regno = regno;
  ir->next = next;
  return ir;
}

/* Free insn reg info list IR.	*/
static void
free_insn_regs (struct lra_insn_reg *ir)
{
  struct lra_insn_reg *next_ir;

  for (; ir != NULL; ir = next_ir)
    {
      next_ir = ir->next;
      lra_insn_reg_pool.remove (ir);
    }
}

/* Finish pool for insn reg info.  */
static void
finish_insn_regs (void)
{
  lra_insn_reg_pool.release ();
}



/* This page contains code dealing LRA insn info (or in other words
   LRA internal insn representation).  */

/* Map INSN_CODE -> the static insn data.  This info is valid during
   all translation unit.  */
struct lra_static_insn_data *insn_code_data[NUM_INSN_CODES];

/* Debug insns are represented as a special insn with one input
   operand which is RTL expression in var_location.  */

/* The following data are used as static insn operand data for all
   debug insns.	 If structure lra_operand_data is changed, the
   initializer should be changed too.  */
static struct lra_operand_data debug_operand_data =
  {
    NULL, /* alternative  */
    0, /* early_clobber_alts */
    E_VOIDmode, /* We are not interesting in the operand mode.  */
    OP_IN,
    0, 0, 0, 0
  };

/* The following data are used as static insn data for all debug
   insns.  If structure lra_static_insn_data is changed, the
   initializer should be changed too.  */
static struct lra_static_insn_data debug_insn_static_data =
  {
    &debug_operand_data,
    0,	/* Duplication operands #.  */
    -1, /* Commutative operand #.  */
    1,	/* Operands #.	There is only one operand which is debug RTL
	   expression.	*/
    0,	/* Duplications #.  */
    0,	/* Alternatives #.  We are not interesting in alternatives
	   because we does not proceed debug_insns for reloads.	 */
    NULL, /* Hard registers referenced in machine description.	*/
    NULL  /* Descriptions of operands in alternatives.	*/
  };

/* Called once per compiler work to initialize some LRA data related
   to insns.  */
static void
init_insn_code_data_once (void)
{
  memset (insn_code_data, 0, sizeof (insn_code_data));
}

/* Called once per compiler work to finalize some LRA data related to
   insns.  */
static void
finish_insn_code_data_once (void)
{
  for (unsigned int i = 0; i < NUM_INSN_CODES; i++)
    {
      if (insn_code_data[i] != NULL)
	free (insn_code_data[i]);
    }
}

/* Return static insn data, allocate and setup if necessary.  Although
   dup_num is static data (it depends only on icode), to set it up we
   need to extract insn first.	So recog_data should be valid for
   normal insn (ICODE >= 0) before the call.  */
static struct lra_static_insn_data *
get_static_insn_data (int icode, int nop, int ndup, int nalt)
{
  struct lra_static_insn_data *data;
  size_t n_bytes;

  lra_assert (icode < (int) NUM_INSN_CODES);
  if (icode >= 0 && (data = insn_code_data[icode]) != NULL)
    return data;
  lra_assert (nop >= 0 && ndup >= 0 && nalt >= 0);
  n_bytes = sizeof (struct lra_static_insn_data)
	    + sizeof (struct lra_operand_data) * nop
	    + sizeof (int) * ndup;
  data = XNEWVAR (struct lra_static_insn_data, n_bytes);
  data->operand_alternative = NULL;
  data->n_operands = nop;
  data->n_dups = ndup;
  data->n_alternatives = nalt;
  data->operand = ((struct lra_operand_data *)
		   ((char *) data + sizeof (struct lra_static_insn_data)));
  data->dup_num = ((int *) ((char *) data->operand
			    + sizeof (struct lra_operand_data) * nop));
  if (icode >= 0)
    {
      int i;

      insn_code_data[icode] = data;
      for (i = 0; i < nop; i++)
	{
	  data->operand[i].constraint
	    = insn_data[icode].operand[i].constraint;
	  data->operand[i].mode = insn_data[icode].operand[i].mode;
	  data->operand[i].strict_low = insn_data[icode].operand[i].strict_low;
	  data->operand[i].is_operator
	    = insn_data[icode].operand[i].is_operator;
	  data->operand[i].type
	    = (data->operand[i].constraint[0] == '=' ? OP_OUT
	       : data->operand[i].constraint[0] == '+' ? OP_INOUT
	       : OP_IN);
	  data->operand[i].is_address = false;
	}
      for (i = 0; i < ndup; i++)
	data->dup_num[i] = recog_data.dup_num[i];
    }
  return data;
}

/* The current length of the following array.  */
int lra_insn_recog_data_len;

/* Map INSN_UID -> the insn recog data (NULL if unknown).  */
lra_insn_recog_data_t *lra_insn_recog_data;

/* Initialize LRA data about insns.  */
static void
init_insn_recog_data (void)
{
  lra_insn_recog_data_len = 0;
  lra_insn_recog_data = NULL;
}

/* Expand, if necessary, LRA data about insns.	*/
static void
check_and_expand_insn_recog_data (int index)
{
  int i, old;

  if (lra_insn_recog_data_len > index)
    return;
  old = lra_insn_recog_data_len;
  lra_insn_recog_data_len = index * 3 / 2 + 1;
  lra_insn_recog_data = XRESIZEVEC (lra_insn_recog_data_t,
				    lra_insn_recog_data,
				    lra_insn_recog_data_len);
  for (i = old; i < lra_insn_recog_data_len; i++)
    lra_insn_recog_data[i] = NULL;
}

/* Finish LRA DATA about insn.	*/
static void
free_insn_recog_data (lra_insn_recog_data_t data)
{
  if (data->operand_loc != NULL)
    free (data->operand_loc);
  if (data->dup_loc != NULL)
    free (data->dup_loc);
  if (data->arg_hard_regs != NULL)
    free (data->arg_hard_regs);
  if (data->icode < 0 && NONDEBUG_INSN_P (data->insn))
    {
      if (data->insn_static_data->operand_alternative != NULL)
	free (const_cast <operand_alternative *>
	      (data->insn_static_data->operand_alternative));
      free_insn_regs (data->insn_static_data->hard_regs);
      free (data->insn_static_data);
    }
  free_insn_regs (data->regs);
  data->regs = NULL;
  free (data);
}

/* Pools for copies.  */
static object_allocator<lra_copy> lra_copy_pool ("lra copies");

/* Finish LRA data about all insns.  */
static void
finish_insn_recog_data (void)
{
  int i;
  lra_insn_recog_data_t data;

  for (i = 0; i < lra_insn_recog_data_len; i++)
    if ((data = lra_insn_recog_data[i]) != NULL)
      free_insn_recog_data (data);
  finish_insn_regs ();
  lra_copy_pool.release ();
  lra_insn_reg_pool.release ();
  free (lra_insn_recog_data);
}

/* Setup info about operands in alternatives of LRA DATA of insn.  */
static void
setup_operand_alternative (lra_insn_recog_data_t data,
			   const operand_alternative *op_alt)
{
  int i, j, nop, nalt;
  int icode = data->icode;
  struct lra_static_insn_data *static_data = data->insn_static_data;

  static_data->commutative = -1;
  nop = static_data->n_operands;
  nalt = static_data->n_alternatives;
  static_data->operand_alternative = op_alt;
  for (i = 0; i < nop; i++)
    {
      static_data->operand[i].early_clobber_alts = 0;
      static_data->operand[i].early_clobber = false;
      static_data->operand[i].is_address = false;
      if (static_data->operand[i].constraint[0] == '%')
	{
	  /* We currently only support one commutative pair of operands.  */
	  if (static_data->commutative < 0)
	    static_data->commutative = i;
	  else
	    lra_assert (icode < 0); /* Asm  */
	  /* The last operand should not be marked commutative.  */
	  lra_assert (i != nop - 1);
	}
    }
  for (j = 0; j < nalt; j++)
    for (i = 0; i < nop; i++, op_alt++)
      {
	static_data->operand[i].early_clobber |= op_alt->earlyclobber;
	if (op_alt->earlyclobber)
	  static_data->operand[i].early_clobber_alts |= (alternative_mask) 1 << j;
	static_data->operand[i].is_address |= op_alt->is_address;
      }
}

/* Recursively process X and collect info about registers, which are
   not the insn operands, in X with TYPE (in/out/inout) and flag that
   it is early clobbered in the insn (EARLY_CLOBBER) and add the info
   to LIST.  X is a part of insn given by DATA.	 Return the result
   list.  */
static struct lra_insn_reg *
collect_non_operand_hard_regs (rtx *x, lra_insn_recog_data_t data,
			       struct lra_insn_reg *list,
			       enum op_type type, bool early_clobber)
{
  int i, j, regno, last;
  bool subreg_p;
  machine_mode mode;
  struct lra_insn_reg *curr;
  rtx op = *x;
  enum rtx_code code = GET_CODE (op);
  const char *fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < data->insn_static_data->n_operands; i++)
    if (! data->insn_static_data->operand[i].is_operator
	&& x == data->operand_loc[i])
      /* It is an operand loc. Stop here.  */
      return list;
  for (i = 0; i < data->insn_static_data->n_dups; i++)
    if (x == data->dup_loc[i])
      /* It is a dup loc. Stop here.  */
      return list;
  mode = GET_MODE (op);
  subreg_p = false;
  if (code == SUBREG)
    {
      op = SUBREG_REG (op);
      code = GET_CODE (op);
      if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (op)))
	{
	  mode = GET_MODE (op);
	  if (GET_MODE_SIZE (mode) > REGMODE_NATURAL_SIZE (mode))
	    subreg_p = true;
	}
    }
  if (REG_P (op))
    {
      if ((regno = REGNO (op)) >= FIRST_PSEUDO_REGISTER)
	return list;
      /* Process all regs even unallocatable ones as we need info
	 about all regs for rematerialization pass.  */
      for (last = end_hard_regno (mode, regno); regno < last; regno++)
	{
	  for (curr = list; curr != NULL; curr = curr->next)
	    if (curr->regno == regno && curr->subreg_p == subreg_p
		&& curr->biggest_mode == mode)
	      {
		if (curr->type != type)
		  curr->type = OP_INOUT;
		if (early_clobber)
		  {
		    curr->early_clobber = true;
		    curr->early_clobber_alts = ALL_ALTERNATIVES;
		  }
		break;
	      }
	  if (curr == NULL)
	    {
	      /* This is a new hard regno or the info can not be
		 integrated into the found structure.	 */
#ifdef STACK_REGS
	      early_clobber
		= (early_clobber
		   /* This clobber is to inform popping floating
		      point stack only.  */
		   && ! (FIRST_STACK_REG <= regno
			 && regno <= LAST_STACK_REG));
#endif
	      list = new_insn_reg (data->insn, regno, type, mode, subreg_p,
				   early_clobber,
				   early_clobber ? ALL_ALTERNATIVES : 0, list);
	    }
	}
      return list;
    }
  switch (code)
    {
    case SET:
      list = collect_non_operand_hard_regs (&SET_DEST (op), data,
					    list, OP_OUT, false);
      list = collect_non_operand_hard_regs (&SET_SRC (op), data,
					    list, OP_IN, false);
      break;
    case CLOBBER:
      /* We treat clobber of non-operand hard registers as early
	 clobber (the behavior is expected from asm).  */
      list = collect_non_operand_hard_regs (&XEXP (op, 0), data,
					    list, OP_OUT, true);
      break;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC:
      list = collect_non_operand_hard_regs (&XEXP (op, 0), data,
					    list, OP_INOUT, false);
      break;
    case PRE_MODIFY: case POST_MODIFY:
      list = collect_non_operand_hard_regs (&XEXP (op, 0), data,
					    list, OP_INOUT, false);
      list = collect_non_operand_hard_regs (&XEXP (op, 1), data,
					    list, OP_IN, false);
      break;
    default:
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    list = collect_non_operand_hard_regs (&XEXP (op, i), data,
						  list, OP_IN, false);
	  else if (fmt[i] == 'E')
	    for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	      list = collect_non_operand_hard_regs (&XVECEXP (op, i, j), data,
						    list, OP_IN, false);
	}
    }
  return list;
}

/* Set up and return info about INSN.  Set up the info if it is not set up
   yet.	 */
lra_insn_recog_data_t
lra_set_insn_recog_data (rtx_insn *insn)
{
  lra_insn_recog_data_t data;
  int i, n, icode;
  rtx **locs;
  unsigned int uid = INSN_UID (insn);
  struct lra_static_insn_data *insn_static_data;

  check_and_expand_insn_recog_data (uid);
  if (DEBUG_INSN_P (insn))
    icode = -1;
  else
    {
      icode = INSN_CODE (insn);
      if (icode < 0)
	/* It might be a new simple insn which is not recognized yet.  */
	INSN_CODE (insn) = icode = recog_memoized (insn);
    }
  data = XNEW (struct lra_insn_recog_data);
  lra_insn_recog_data[uid] = data;
  data->insn = insn;
  data->used_insn_alternative = -1;
  data->icode = icode;
  data->regs = NULL;
  if (DEBUG_INSN_P (insn))
    {
      data->insn_static_data = &debug_insn_static_data;
      data->dup_loc = NULL;
      data->arg_hard_regs = NULL;
      data->preferred_alternatives = ALL_ALTERNATIVES;
      data->operand_loc = XNEWVEC (rtx *, 1);
      data->operand_loc[0] = &INSN_VAR_LOCATION_LOC (insn);
      return data;
    }
  if (icode < 0)
    {
      int nop, nalt;
      machine_mode operand_mode[MAX_RECOG_OPERANDS];
      const char *constraints[MAX_RECOG_OPERANDS];

      nop = asm_noperands (PATTERN (insn));
      data->operand_loc = data->dup_loc = NULL;
      nalt = 1;
      if (nop < 0)
	{
	  /* It is a special insn like USE or CLOBBER.  We should
	     recognize any regular insn otherwise LRA can do nothing
	     with this insn.  */
	  gcc_assert (GET_CODE (PATTERN (insn)) == USE
		      || GET_CODE (PATTERN (insn)) == CLOBBER
		      || GET_CODE (PATTERN (insn)) == ASM_INPUT);
	  data->insn_static_data = insn_static_data
	    = get_static_insn_data (-1, 0, 0, nalt);
	}
      else
	{
	  /* expand_asm_operands makes sure there aren't too many
	     operands.	*/
	  lra_assert (nop <= MAX_RECOG_OPERANDS);
	  if (nop != 0)
	    data->operand_loc = XNEWVEC (rtx *, nop);
	  /* Now get the operand values and constraints out of the
	     insn.  */
	  decode_asm_operands (PATTERN (insn), NULL,
			       data->operand_loc,
			       constraints, operand_mode, NULL);
	  if (nop > 0)
	    {
	      const char *p =  recog_data.constraints[0];

	      for (p =	constraints[0]; *p; p++)
		nalt += *p == ',';
	    }
	  data->insn_static_data = insn_static_data
	    = get_static_insn_data (-1, nop, 0, nalt);
	  for (i = 0; i < nop; i++)
	    {
	      insn_static_data->operand[i].mode = operand_mode[i];
	      insn_static_data->operand[i].constraint = constraints[i];
	      insn_static_data->operand[i].strict_low = false;
	      insn_static_data->operand[i].is_operator = false;
	      insn_static_data->operand[i].is_address = false;
	    }
	}
      for (i = 0; i < insn_static_data->n_operands; i++)
	insn_static_data->operand[i].type
	  = (insn_static_data->operand[i].constraint[0] == '=' ? OP_OUT
	     : insn_static_data->operand[i].constraint[0] == '+' ? OP_INOUT
	     : OP_IN);
      data->preferred_alternatives = ALL_ALTERNATIVES;
      if (nop > 0)
	{
	  operand_alternative *op_alt = XCNEWVEC (operand_alternative,
						  nalt * nop);
	  preprocess_constraints (nop, nalt, constraints, op_alt);
	  setup_operand_alternative (data, op_alt);
	}
    }
  else
    {
      insn_extract (insn);
      data->insn_static_data = insn_static_data
	= get_static_insn_data (icode, insn_data[icode].n_operands,
				insn_data[icode].n_dups,
				insn_data[icode].n_alternatives);
      n = insn_static_data->n_operands;
      if (n == 0)
	locs = NULL;
      else
	{
	  locs = XNEWVEC (rtx *, n);
	  memcpy (locs, recog_data.operand_loc, n * sizeof (rtx *));
	}
      data->operand_loc = locs;
      n = insn_static_data->n_dups;
      if (n == 0)
	locs = NULL;
      else
	{
	  locs = XNEWVEC (rtx *, n);
	  memcpy (locs, recog_data.dup_loc, n * sizeof (rtx *));
	}
      data->dup_loc = locs;
      data->preferred_alternatives = get_preferred_alternatives (insn);
      const operand_alternative *op_alt = preprocess_insn_constraints (icode);
      if (!insn_static_data->operand_alternative)
	setup_operand_alternative (data, op_alt);
      else if (op_alt != insn_static_data->operand_alternative)
	insn_static_data->operand_alternative = op_alt;
    }
  if (GET_CODE (PATTERN (insn)) == CLOBBER || GET_CODE (PATTERN (insn)) == USE)
    insn_static_data->hard_regs = NULL;
  else
    insn_static_data->hard_regs
      = collect_non_operand_hard_regs (&PATTERN (insn), data,
				       NULL, OP_IN, false);
  data->arg_hard_regs = NULL;
  if (CALL_P (insn))
    {
      bool use_p;
      rtx link;
      int n_hard_regs, regno, arg_hard_regs[FIRST_PSEUDO_REGISTER];

      n_hard_regs = 0;
      /* Finding implicit hard register usage.	We believe it will be
	 not changed whatever transformations are used.	 Call insns
	 are such example.  */
      for (link = CALL_INSN_FUNCTION_USAGE (insn);
	   link != NULL_RTX;
	   link = XEXP (link, 1))
	if (((use_p = GET_CODE (XEXP (link, 0)) == USE)
	     || GET_CODE (XEXP (link, 0)) == CLOBBER)
	    && REG_P (XEXP (XEXP (link, 0), 0)))
	  {
	    regno = REGNO (XEXP (XEXP (link, 0), 0));
	    lra_assert (regno < FIRST_PSEUDO_REGISTER);
	    /* It is an argument register.  */
	    for (i = REG_NREGS (XEXP (XEXP (link, 0), 0)) - 1; i >= 0; i--)
	      arg_hard_regs[n_hard_regs++]
		= regno + i + (use_p ? 0 : FIRST_PSEUDO_REGISTER);
	  }
      if (n_hard_regs != 0)
	{
	  arg_hard_regs[n_hard_regs++] = -1;
	  data->arg_hard_regs = XNEWVEC (int, n_hard_regs);
	  memcpy (data->arg_hard_regs, arg_hard_regs,
		  sizeof (int) * n_hard_regs);
	}
    }
  /* Some output operand can be recognized only from the context not
     from the constraints which are empty in this case.	 Call insn may
     contain a hard register in set destination with empty constraint
     and extract_insn treats them as an input.	*/
  for (i = 0; i < insn_static_data->n_operands; i++)
    {
      int j;
      rtx pat, set;
      struct lra_operand_data *operand = &insn_static_data->operand[i];

      /* ??? Should we treat 'X' the same way.	It looks to me that
	 'X' means anything and empty constraint means we do not
	 care.	*/
      if (operand->type != OP_IN || *operand->constraint != '\0'
	  || operand->is_operator)
	continue;
      pat = PATTERN (insn);
      if (GET_CODE (pat) == SET)
	{
	  if (data->operand_loc[i] != &SET_DEST (pat))
	    continue;
	}
      else if (GET_CODE (pat) == PARALLEL)
	{
	  for (j = XVECLEN (pat, 0) - 1; j >= 0; j--)
	    {
	      set = XVECEXP (PATTERN (insn), 0, j);
	      if (GET_CODE (set) == SET
		  && &SET_DEST (set) == data->operand_loc[i])
		break;
	    }
	  if (j < 0)
	    continue;
	}
      else
	continue;
      operand->type = OP_OUT;
    }
  return data;
}

/* Return info about insn give by UID.	The info should be already set
   up.	*/
static lra_insn_recog_data_t
get_insn_recog_data_by_uid (int uid)
{
  lra_insn_recog_data_t data;

  data = lra_insn_recog_data[uid];
  lra_assert (data != NULL);
  return data;
}

/* Invalidate all info about insn given by its UID.  */
static void
invalidate_insn_recog_data (int uid)
{
  lra_insn_recog_data_t data;

  data = lra_insn_recog_data[uid];
  lra_assert (data != NULL);
  free_insn_recog_data (data);
  lra_insn_recog_data[uid] = NULL;
}

/* Update all the insn info about INSN.	 It is usually called when
   something in the insn was changed.  Return the updated info.	 */
lra_insn_recog_data_t
lra_update_insn_recog_data (rtx_insn *insn)
{
  lra_insn_recog_data_t data;
  int n;
  unsigned int uid = INSN_UID (insn);
  struct lra_static_insn_data *insn_static_data;
  HOST_WIDE_INT sp_offset = 0;

  check_and_expand_insn_recog_data (uid);
  if ((data = lra_insn_recog_data[uid]) != NULL
      && data->icode != INSN_CODE (insn))
    {
      sp_offset = data->sp_offset;
      invalidate_insn_data_regno_info (data, insn, get_insn_freq (insn));
      invalidate_insn_recog_data (uid);
      data = NULL;
    }
  if (data == NULL)
    {
      data = lra_get_insn_recog_data (insn);
      /* Initiate or restore SP offset.  */
      data->sp_offset = sp_offset;
      return data;
    }
  insn_static_data = data->insn_static_data;
  data->used_insn_alternative = -1;
  if (DEBUG_INSN_P (insn))
    return data;
  if (data->icode < 0)
    {
      int nop;
      machine_mode operand_mode[MAX_RECOG_OPERANDS];
      const char *constraints[MAX_RECOG_OPERANDS];

      nop = asm_noperands (PATTERN (insn));
      if (nop >= 0)
	{
	  lra_assert (nop == data->insn_static_data->n_operands);
	  /* Now get the operand values and constraints out of the
	     insn.  */
	  decode_asm_operands (PATTERN (insn), NULL,
			       data->operand_loc,
			       constraints, operand_mode, NULL);

	  if (flag_checking)
	    for (int i = 0; i < nop; i++)
	      lra_assert
		(insn_static_data->operand[i].mode == operand_mode[i]
		 && insn_static_data->operand[i].constraint == constraints[i]
		 && ! insn_static_data->operand[i].is_operator);
	}

      if (flag_checking)
	for (int i = 0; i < insn_static_data->n_operands; i++)
	  lra_assert
	    (insn_static_data->operand[i].type
	     == (insn_static_data->operand[i].constraint[0] == '=' ? OP_OUT
		 : insn_static_data->operand[i].constraint[0] == '+' ? OP_INOUT
		 : OP_IN));
    }
  else
    {
      insn_extract (insn);
      n = insn_static_data->n_operands;
      if (n != 0)
	memcpy (data->operand_loc, recog_data.operand_loc, n * sizeof (rtx *));
      n = insn_static_data->n_dups;
      if (n != 0)
	memcpy (data->dup_loc, recog_data.dup_loc, n * sizeof (rtx *));
      lra_assert (check_bool_attrs (insn));
    }
  return data;
}

/* Set up that INSN is using alternative ALT now.  */
void
lra_set_used_insn_alternative (rtx_insn *insn, int alt)
{
  lra_insn_recog_data_t data;

  data = lra_get_insn_recog_data (insn);
  data->used_insn_alternative = alt;
}

/* Set up that insn with UID is using alternative ALT now.  The insn
   info should be already set up.  */
void
lra_set_used_insn_alternative_by_uid (int uid, int alt)
{
  lra_insn_recog_data_t data;

  check_and_expand_insn_recog_data (uid);
  data = lra_insn_recog_data[uid];
  lra_assert (data != NULL);
  data->used_insn_alternative = alt;
}



/* This page contains code dealing with common register info and
   pseudo copies.  */

/* The size of the following array.  */
static int reg_info_size;
/* Common info about each register.  */
struct lra_reg *lra_reg_info;

/* Last register value.	 */
static int last_reg_value;

/* Return new register value.  */
static int
get_new_reg_value (void)
{
  return ++last_reg_value;
}

/* Vec referring to pseudo copies.  */
static vec<lra_copy_t> copy_vec;

/* Initialize I-th element of lra_reg_info.  */
static inline void
initialize_lra_reg_info_element (int i)
{
  bitmap_initialize (&lra_reg_info[i].insn_bitmap, &reg_obstack);
#ifdef STACK_REGS
  lra_reg_info[i].no_stack_p = false;
#endif
  CLEAR_HARD_REG_SET (lra_reg_info[i].conflict_hard_regs);
  CLEAR_HARD_REG_SET (lra_reg_info[i].actual_call_used_reg_set);
  lra_reg_info[i].preferred_hard_regno1 = -1;
  lra_reg_info[i].preferred_hard_regno2 = -1;
  lra_reg_info[i].preferred_hard_regno_profit1 = 0;
  lra_reg_info[i].preferred_hard_regno_profit2 = 0;
  lra_reg_info[i].biggest_mode = VOIDmode;
  lra_reg_info[i].live_ranges = NULL;
  lra_reg_info[i].nrefs = lra_reg_info[i].freq = 0;
  lra_reg_info[i].last_reload = 0;
  lra_reg_info[i].restore_rtx = NULL_RTX;
  lra_reg_info[i].val = get_new_reg_value ();
  lra_reg_info[i].offset = 0;
  lra_reg_info[i].copies = NULL;
}

/* Initialize common reg info and copies.  */
static void
init_reg_info (void)
{
  int i;

  last_reg_value = 0;
  reg_info_size = max_reg_num () * 3 / 2 + 1;
  lra_reg_info = XNEWVEC (struct lra_reg, reg_info_size);
  for (i = 0; i < reg_info_size; i++)
    initialize_lra_reg_info_element (i);
  copy_vec.truncate (0);
}


/* Finish common reg info and copies.  */
static void
finish_reg_info (void)
{
  int i;

  for (i = 0; i < reg_info_size; i++)
    bitmap_clear (&lra_reg_info[i].insn_bitmap);
  free (lra_reg_info);
  reg_info_size = 0;
}

/* Expand common reg info if it is necessary.  */
static void
expand_reg_info (void)
{
  int i, old = reg_info_size;

  if (reg_info_size > max_reg_num ())
    return;
  reg_info_size = max_reg_num () * 3 / 2 + 1;
  lra_reg_info = XRESIZEVEC (struct lra_reg, lra_reg_info, reg_info_size);
  for (i = old; i < reg_info_size; i++)
    initialize_lra_reg_info_element (i);
}

/* Free all copies.  */
void
lra_free_copies (void)
{
  lra_copy_t cp;

  while (copy_vec.length () != 0)
    {
      cp = copy_vec.pop ();
      lra_reg_info[cp->regno1].copies = lra_reg_info[cp->regno2].copies = NULL;
      lra_copy_pool.remove (cp);
    }
}

/* Create copy of two pseudos REGNO1 and REGNO2.  The copy execution
   frequency is FREQ.  */
void
lra_create_copy (int regno1, int regno2, int freq)
{
  bool regno1_dest_p;
  lra_copy_t cp;

  lra_assert (regno1 != regno2);
  regno1_dest_p = true;
  if (regno1 > regno2)
    {
      std::swap (regno1, regno2);
      regno1_dest_p = false;
    }
  cp = lra_copy_pool.allocate ();
  copy_vec.safe_push (cp);
  cp->regno1_dest_p = regno1_dest_p;
  cp->freq = freq;
  cp->regno1 = regno1;
  cp->regno2 = regno2;
  cp->regno1_next = lra_reg_info[regno1].copies;
  lra_reg_info[regno1].copies = cp;
  cp->regno2_next = lra_reg_info[regno2].copies;
  lra_reg_info[regno2].copies = cp;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "	   Creating copy r%d%sr%d@%d\n",
	     regno1, regno1_dest_p ? "<-" : "->", regno2, freq);
}

/* Return N-th (0, 1, ...) copy.  If there is no copy, return
   NULL.  */
lra_copy_t
lra_get_copy (int n)
{
  if (n >= (int) copy_vec.length ())
    return NULL;
  return copy_vec[n];
}



/* This page contains code dealing with info about registers in
   insns.  */

/* Process X of insn UID recursively and add info (operand type is
   given by TYPE, flag of that it is early clobber is EARLY_CLOBBER)
   about registers in X to the insn DATA.  If X can be early clobbered,
   alternatives in which it can be early clobbered are given by
   EARLY_CLOBBER_ALTS.  */
static void
add_regs_to_insn_regno_info (lra_insn_recog_data_t data, rtx x, int uid,
			     enum op_type type, bool early_clobber,
			     alternative_mask early_clobber_alts)
{
  int i, j, regno;
  bool subreg_p;
  machine_mode mode;
  const char *fmt;
  enum rtx_code code;
  struct lra_insn_reg *curr;

  code = GET_CODE (x);
  mode = GET_MODE (x);
  subreg_p = false;
  if (GET_CODE (x) == SUBREG)
    {
      x = SUBREG_REG (x);
      code = GET_CODE (x);
      if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (x)))
	{
	  mode = GET_MODE (x);
	  if (GET_MODE_SIZE (mode) > REGMODE_NATURAL_SIZE (mode))
	    subreg_p = true;
	}
    }
  if (REG_P (x))
    {
      regno = REGNO (x);
      /* Process all regs even unallocatable ones as we need info about
	 all regs for rematerialization pass.  */
      expand_reg_info ();
      if (bitmap_set_bit (&lra_reg_info[regno].insn_bitmap, uid))
	{
	  data->regs = new_insn_reg (data->insn, regno, type, mode, subreg_p,
				     early_clobber, early_clobber_alts,
				     data->regs);
	  return;
	}
      else
	{
	  for (curr = data->regs; curr != NULL; curr = curr->next)
	    if (curr->regno == regno)
	      {
		if (curr->subreg_p != subreg_p || curr->biggest_mode != mode)
		  /* The info can not be integrated into the found
		     structure.  */
		  data->regs = new_insn_reg (data->insn, regno, type, mode,
					     subreg_p, early_clobber,
					     early_clobber_alts, data->regs);
		else
		  {
		    if (curr->type != type)
		      curr->type = OP_INOUT;
		    if (curr->early_clobber != early_clobber)
		      curr->early_clobber = true;
		    curr->early_clobber_alts |= early_clobber_alts;
		  }
		return;
	      }
	  gcc_unreachable ();
	}
    }

  switch (code)
    {
    case SET:
      add_regs_to_insn_regno_info (data, SET_DEST (x), uid, OP_OUT, false, 0);
      add_regs_to_insn_regno_info (data, SET_SRC (x), uid, OP_IN, false, 0);
      break;
    case CLOBBER:
      /* We treat clobber of non-operand hard registers as early
	 clobber (the behavior is expected from asm).  */
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_OUT, true, ALL_ALTERNATIVES);
      break;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC:
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_INOUT, false, 0);
      break;
    case PRE_MODIFY: case POST_MODIFY:
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_INOUT, false, 0);
      add_regs_to_insn_regno_info (data, XEXP (x, 1), uid, OP_IN, false, 0);
      break;
    default:
      if ((code != PARALLEL && code != EXPR_LIST) || type != OP_OUT)
	/* Some targets place small structures in registers for return
	   values of functions, and those registers are wrapped in
	   PARALLEL that we may see as the destination of a SET.  Here
	   is an example:

	   (call_insn 13 12 14 2 (set (parallel:BLK [
		(expr_list:REG_DEP_TRUE (reg:DI 0 ax)
		    (const_int 0 [0]))
		(expr_list:REG_DEP_TRUE (reg:DI 1 dx)
		    (const_int 8 [0x8]))
	       ])
	     (call (mem:QI (symbol_ref:DI (...	*/
	type = OP_IN;
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    add_regs_to_insn_regno_info (data, XEXP (x, i), uid, type, false, 0);
	  else if (fmt[i] == 'E')
	    {
	      for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		add_regs_to_insn_regno_info (data, XVECEXP (x, i, j), uid,
					     type, false, 0);
	    }
	}
    }
}

/* Return execution frequency of INSN.	*/
static int
get_insn_freq (rtx_insn *insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);

  gcc_checking_assert (bb != NULL);
  return REG_FREQ_FROM_BB (bb);
}

/* Invalidate all reg info of INSN with DATA and execution frequency
   FREQ.  Update common info about the invalidated registers.  */
static void
invalidate_insn_data_regno_info (lra_insn_recog_data_t data, rtx_insn *insn,
				 int freq)
{
  int uid;
  bool debug_p;
  unsigned int i;
  struct lra_insn_reg *ir, *next_ir;

  uid = INSN_UID (insn);
  debug_p = DEBUG_INSN_P (insn);
  for (ir = data->regs; ir != NULL; ir = next_ir)
    {
      i = ir->regno;
      next_ir = ir->next;
      lra_insn_reg_pool.remove (ir);
      bitmap_clear_bit (&lra_reg_info[i].insn_bitmap, uid);
      if (i >= FIRST_PSEUDO_REGISTER && ! debug_p)
	{
	  lra_reg_info[i].nrefs--;
	  lra_reg_info[i].freq -= freq;
	  lra_assert (lra_reg_info[i].nrefs >= 0 && lra_reg_info[i].freq >= 0);
	}
    }
  data->regs = NULL;
}

/* Invalidate all reg info of INSN.  Update common info about the
   invalidated registers.  */
void
lra_invalidate_insn_regno_info (rtx_insn *insn)
{
  invalidate_insn_data_regno_info (lra_get_insn_recog_data (insn), insn,
				   get_insn_freq (insn));
}

/* Update common reg info from reg info of insn given by its DATA and
   execution frequency FREQ.  */
static void
setup_insn_reg_info (lra_insn_recog_data_t data, int freq)
{
  unsigned int i;
  struct lra_insn_reg *ir;

  for (ir = data->regs; ir != NULL; ir = ir->next)
    if ((i = ir->regno) >= FIRST_PSEUDO_REGISTER)
      {
	lra_reg_info[i].nrefs++;
	lra_reg_info[i].freq += freq;
      }
}

/* Set up insn reg info of INSN.  Update common reg info from reg info
   of INSN.  */
void
lra_update_insn_regno_info (rtx_insn *insn)
{
  int i, uid, freq;
  lra_insn_recog_data_t data;
  struct lra_static_insn_data *static_data;
  enum rtx_code code;
  rtx link;
  
  if (! INSN_P (insn))
    return;
  data = lra_get_insn_recog_data (insn);
  static_data = data->insn_static_data;
  freq = get_insn_freq (insn);
  invalidate_insn_data_regno_info (data, insn, freq);
  uid = INSN_UID (insn);
  for (i = static_data->n_operands - 1; i >= 0; i--)
    add_regs_to_insn_regno_info (data, *data->operand_loc[i], uid,
				 static_data->operand[i].type,
				 static_data->operand[i].early_clobber,
				 static_data->operand[i].early_clobber_alts);
  if ((code = GET_CODE (PATTERN (insn))) == CLOBBER || code == USE)
    add_regs_to_insn_regno_info (data, XEXP (PATTERN (insn), 0), uid,
				 code == USE ? OP_IN : OP_OUT, false, 0);
  if (CALL_P (insn))
    /* On some targets call insns can refer to pseudos in memory in
       CALL_INSN_FUNCTION_USAGE list.  Process them in order to
       consider their occurrences in calls for different
       transformations (e.g. inheritance) with given pseudos.  */
    for (link = CALL_INSN_FUNCTION_USAGE (insn);
	 link != NULL_RTX;
	 link = XEXP (link, 1))
      if (((code = GET_CODE (XEXP (link, 0))) == USE || code == CLOBBER)
	  && MEM_P (XEXP (XEXP (link, 0), 0)))
	add_regs_to_insn_regno_info (data, XEXP (XEXP (link, 0), 0), uid,
				     code == USE ? OP_IN : OP_OUT, false, 0);
  if (NONDEBUG_INSN_P (insn))
    setup_insn_reg_info (data, freq);
}

/* Return reg info of insn given by it UID.  */
struct lra_insn_reg *
lra_get_insn_regs (int uid)
{
  lra_insn_recog_data_t data;

  data = get_insn_recog_data_by_uid (uid);
  return data->regs;
}



/* Recursive hash function for RTL X.  */
hashval_t
lra_rtx_hash (rtx x)
{
  int i, j;
  enum rtx_code code;
  const char *fmt;
  hashval_t val = 0;

  if (x == 0)
    return val;

  code = GET_CODE (x);
  val += (int) code + 4095;

  /* Some RTL can be compared nonrecursively.  */
  switch (code)
    {
    case REG:
      return val + REGNO (x);

    case LABEL_REF:
      return iterative_hash_object (XEXP (x, 0), val);

    case SYMBOL_REF:
      return iterative_hash_object (XSTR (x, 0), val);

    case SCRATCH:
    case CONST_DOUBLE:
    case CONST_INT:
    case CONST_VECTOR:
      return val;

    default:
      break;
    }

  /* Hash the elements.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'w':
	  val += XWINT (x, i);
	  break;

	case 'n':
	case 'i':
	  val += XINT (x, i);
	  break;

	case 'V':
	case 'E':
	  val += XVECLEN (x, i);

	  for (j = 0; j < XVECLEN (x, i); j++)
	    val += lra_rtx_hash (XVECEXP (x, i, j));
	  break;

	case 'e':
	  val += lra_rtx_hash (XEXP (x, i));
	  break;

	case 'S':
	case 's':
	  val += htab_hash_string (XSTR (x, i));
	  break;

	case 'u':
	case '0':
	case 't':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's, except for
	     within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return val;
}



/* This page contains code dealing with stack of the insns which
   should be processed by the next constraint pass.  */

/* Bitmap used to put an insn on the stack only in one exemplar.  */
static sbitmap lra_constraint_insn_stack_bitmap;

/* The stack itself.  */
vec<rtx_insn *> lra_constraint_insn_stack;

/* Put INSN on the stack.  If ALWAYS_UPDATE is true, always update the reg
   info for INSN, otherwise only update it if INSN is not already on the
   stack.  */
static inline void
lra_push_insn_1 (rtx_insn *insn, bool always_update)
{
  unsigned int uid = INSN_UID (insn);
  if (always_update)
    lra_update_insn_regno_info (insn);
  if (uid >= SBITMAP_SIZE (lra_constraint_insn_stack_bitmap))
    lra_constraint_insn_stack_bitmap =
      sbitmap_resize (lra_constraint_insn_stack_bitmap, 3 * uid / 2, 0);
  if (bitmap_bit_p (lra_constraint_insn_stack_bitmap, uid))
    return;
  bitmap_set_bit (lra_constraint_insn_stack_bitmap, uid);
  if (! always_update)
    lra_update_insn_regno_info (insn);
  lra_constraint_insn_stack.safe_push (insn);
}

/* Put INSN on the stack.  */
void
lra_push_insn (rtx_insn *insn)
{
  lra_push_insn_1 (insn, false);
}

/* Put INSN on the stack and update its reg info.  */
void
lra_push_insn_and_update_insn_regno_info (rtx_insn *insn)
{
  lra_push_insn_1 (insn, true);
}

/* Put insn with UID on the stack.  */
void
lra_push_insn_by_uid (unsigned int uid)
{
  lra_push_insn (lra_insn_recog_data[uid]->insn);
}

/* Take the last-inserted insns off the stack and return it.  */
rtx_insn *
lra_pop_insn (void)
{
  rtx_insn *insn = lra_constraint_insn_stack.pop ();
  bitmap_clear_bit (lra_constraint_insn_stack_bitmap, INSN_UID (insn));
  return insn;
}

/* Return the current size of the insn stack.  */
unsigned int
lra_insn_stack_length (void)
{
  return lra_constraint_insn_stack.length ();
}

/* Push insns FROM to TO (excluding it) going in reverse order.	 */
static void
push_insns (rtx_insn *from, rtx_insn *to)
{
  rtx_insn *insn;

  if (from == NULL_RTX)
    return;
  for (insn = from; insn != to; insn = PREV_INSN (insn))
    if (INSN_P (insn))
      lra_push_insn (insn);
}

/* Set up sp offset for insn in range [FROM, LAST].  The offset is
   taken from the next BB insn after LAST or zero if there in such
   insn.  */
static void
setup_sp_offset (rtx_insn *from, rtx_insn *last)
{
  rtx_insn *before = next_nonnote_insn_bb (last);
  HOST_WIDE_INT offset = (before == NULL_RTX || ! INSN_P (before)
			  ? 0 : lra_get_insn_recog_data (before)->sp_offset);

  for (rtx_insn *insn = from; insn != NEXT_INSN (last); insn = NEXT_INSN (insn))
    lra_get_insn_recog_data (insn)->sp_offset = offset;
}

/* Emit insns BEFORE before INSN and insns AFTER after INSN.  Put the
   insns onto the stack.  Print about emitting the insns with
   TITLE.  */
void
lra_process_new_insns (rtx_insn *insn, rtx_insn *before, rtx_insn *after,
		       const char *title)
{
  rtx_insn *last;

  if (before == NULL_RTX && after == NULL_RTX)
    return;
  if (lra_dump_file != NULL)
    {
      dump_insn_slim (lra_dump_file, insn);
      if (before != NULL_RTX)
	{
	  fprintf (lra_dump_file,"    %s before:\n", title);
	  dump_rtl_slim (lra_dump_file, before, NULL, -1, 0);
	}
      if (after != NULL_RTX)
	{
	  fprintf (lra_dump_file, "    %s after:\n", title);
	  dump_rtl_slim (lra_dump_file, after, NULL, -1, 0);
	}
      fprintf (lra_dump_file, "\n");
    }
  if (before != NULL_RTX)
    {
      if (cfun->can_throw_non_call_exceptions)
	copy_reg_eh_region_note_forward (insn, before, NULL);
      emit_insn_before (before, insn);
      push_insns (PREV_INSN (insn), PREV_INSN (before));
      setup_sp_offset (before, PREV_INSN (insn));
    }
  if (after != NULL_RTX)
    {
      if (cfun->can_throw_non_call_exceptions)
	copy_reg_eh_region_note_forward (insn, after, NULL);
      for (last = after; NEXT_INSN (last) != NULL_RTX; last = NEXT_INSN (last))
	;
      emit_insn_after (after, insn);
      push_insns (last, insn);
      setup_sp_offset (after, last);
    }
  if (cfun->can_throw_non_call_exceptions)
    {
      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
      if (note && !insn_could_throw_p (insn))
	remove_note (insn, note);
    }
}


/* Replace all references to register OLD_REGNO in *LOC with pseudo
   register NEW_REG.  Try to simplify subreg of constant if SUBREG_P.
   Return true if any change was made.  */
bool
lra_substitute_pseudo (rtx *loc, int old_regno, rtx new_reg, bool subreg_p)
{
  rtx x = *loc;
  bool result = false;
  enum rtx_code code;
  const char *fmt;
  int i, j;

  if (x == NULL_RTX)
    return false;

  code = GET_CODE (x);
  if (code == SUBREG && subreg_p)
    {
      rtx subst, inner = SUBREG_REG (x);
      /* Transform subreg of constant while we still have inner mode
	 of the subreg.  The subreg internal should not be an insn
	 operand.  */
      if (REG_P (inner) && (int) REGNO (inner) == old_regno
	  && CONSTANT_P (new_reg)
	  && (subst = simplify_subreg (GET_MODE (x), new_reg, GET_MODE (inner),
				       SUBREG_BYTE (x))) != NULL_RTX)
	{
	  *loc = subst;
	  return true;
	}
      
    }
  else if (code == REG && (int) REGNO (x) == old_regno)
    {
      machine_mode mode = GET_MODE (x);
      machine_mode inner_mode = GET_MODE (new_reg);

      if (mode != inner_mode
	  && ! (CONST_INT_P (new_reg) && SCALAR_INT_MODE_P (mode)))
	{
	  if (!partial_subreg_p (mode, inner_mode)
	      || ! SCALAR_INT_MODE_P (inner_mode))
	    new_reg = gen_rtx_SUBREG (mode, new_reg, 0);
	  else
	    new_reg = gen_lowpart_SUBREG (mode, new_reg);
	}
      *loc = new_reg;
      return true;
    }

  /* Scan all the operand sub-expressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (lra_substitute_pseudo (&XEXP (x, i), old_regno,
				     new_reg, subreg_p))
	    result = true;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (lra_substitute_pseudo (&XVECEXP (x, i, j), old_regno,
				       new_reg, subreg_p))
	      result = true;
	}
    }
  return result;
}

/* Call lra_substitute_pseudo within an insn.  Try to simplify subreg
   of constant if SUBREG_P.  This won't update the insn ptr, just the
   contents of the insn.  */
bool
lra_substitute_pseudo_within_insn (rtx_insn *insn, int old_regno,
				   rtx new_reg, bool subreg_p)
{
  rtx loc = insn;
  return lra_substitute_pseudo (&loc, old_regno, new_reg, subreg_p);
}



/* This page contains code dealing with scratches (changing them onto
   pseudos and restoring them from the pseudos).

   We change scratches into pseudos at the beginning of LRA to
   simplify dealing with them (conflicts, hard register assignments).

   If the pseudo denoting scratch was spilled it means that we do need
   a hard register for it.  Such pseudos are transformed back to
   scratches at the end of LRA.	 */

/* Description of location of a former scratch operand.	 */
struct sloc
{
  rtx_insn *insn; /* Insn where the scratch was.  */
  int nop;  /* Number of the operand which was a scratch.  */
};

typedef struct sloc *sloc_t;

/* Locations of the former scratches.  */
static vec<sloc_t> scratches;

/* Bitmap of scratch regnos.  */
static bitmap_head scratch_bitmap;

/* Bitmap of scratch operands.	*/
static bitmap_head scratch_operand_bitmap;

/* Return true if pseudo REGNO is made of SCRATCH.  */
bool
lra_former_scratch_p (int regno)
{
  return bitmap_bit_p (&scratch_bitmap, regno);
}

/* Return true if the operand NOP of INSN is a former scratch.	*/
bool
lra_former_scratch_operand_p (rtx_insn *insn, int nop)
{
  return bitmap_bit_p (&scratch_operand_bitmap,
		       INSN_UID (insn) * MAX_RECOG_OPERANDS + nop) != 0;
}

/* Register operand NOP in INSN as a former scratch.  It will be
   changed to scratch back, if it is necessary, at the LRA end.  */
void
lra_register_new_scratch_op (rtx_insn *insn, int nop)
{
  lra_insn_recog_data_t id = lra_get_insn_recog_data (insn);
  rtx op = *id->operand_loc[nop];
  sloc_t loc = XNEW (struct sloc);
  lra_assert (REG_P (op));
  loc->insn = insn;
  loc->nop = nop;
  scratches.safe_push (loc);
  bitmap_set_bit (&scratch_bitmap, REGNO (op));
  bitmap_set_bit (&scratch_operand_bitmap,
		  INSN_UID (insn) * MAX_RECOG_OPERANDS + nop);
  add_reg_note (insn, REG_UNUSED, op);
}

/* Change scratches onto pseudos and save their location.  */
static void
remove_scratches (void)
{
  int i;
  bool insn_changed_p;
  basic_block bb;
  rtx_insn *insn;
  rtx reg;
  lra_insn_recog_data_t id;
  struct lra_static_insn_data *static_id;

  scratches.create (get_max_uid ());
  bitmap_initialize (&scratch_bitmap, &reg_obstack);
  bitmap_initialize (&scratch_operand_bitmap, &reg_obstack);
  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
	id = lra_get_insn_recog_data (insn);
	static_id = id->insn_static_data;
	insn_changed_p = false;
	for (i = 0; i < static_id->n_operands; i++)
	  if (GET_CODE (*id->operand_loc[i]) == SCRATCH
	      && GET_MODE (*id->operand_loc[i]) != VOIDmode)
	    {
	      insn_changed_p = true;
	      *id->operand_loc[i] = reg
		= lra_create_new_reg (static_id->operand[i].mode,
				      *id->operand_loc[i], ALL_REGS, NULL);
	      lra_register_new_scratch_op (insn, i);
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file,
			 "Removing SCRATCH in insn #%u (nop %d)\n",
			 INSN_UID (insn), i);
	    }
	if (insn_changed_p)
	  /* Because we might use DF right after caller-saves sub-pass
	     we need to keep DF info up to date.  */
	  df_insn_rescan (insn);
      }
}

/* Changes pseudos created by function remove_scratches onto scratches.	 */
static void
restore_scratches (void)
{
  int regno;
  unsigned i;
  sloc_t loc;
  rtx_insn *last = NULL;
  lra_insn_recog_data_t id = NULL;

  for (i = 0; scratches.iterate (i, &loc); i++)
    {
      /* Ignore already deleted insns.  */
      if (NOTE_P (loc->insn)
	  && NOTE_KIND (loc->insn) == NOTE_INSN_DELETED)
	continue;
      if (last != loc->insn)
	{
	  last = loc->insn;
	  id = lra_get_insn_recog_data (last);
	}
      if (REG_P (*id->operand_loc[loc->nop])
	  && ((regno = REGNO (*id->operand_loc[loc->nop]))
	      >= FIRST_PSEUDO_REGISTER)
	  && lra_get_regno_hard_regno (regno) < 0)
	{
	  /* It should be only case when scratch register with chosen
	     constraint 'X' did not get memory or hard register.  */
	  lra_assert (lra_former_scratch_p (regno));
	  *id->operand_loc[loc->nop]
	    = gen_rtx_SCRATCH (GET_MODE (*id->operand_loc[loc->nop]));
	  lra_update_dup (id, loc->nop);
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, "Restoring SCRATCH in insn #%u(nop %d)\n",
		     INSN_UID (loc->insn), loc->nop);
	}
    }
  for (i = 0; scratches.iterate (i, &loc); i++)
    free (loc);
  scratches.release ();
  bitmap_clear (&scratch_bitmap);
  bitmap_clear (&scratch_operand_bitmap);
}



/* Function checks RTL for correctness.	 If FINAL_P is true, it is
   done at the end of LRA and the check is more rigorous.  */
static void
check_rtl (bool final_p)
{
  basic_block bb;
  rtx_insn *insn;

  lra_assert (! final_p || reload_completed);
  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn)
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER
	&& GET_CODE (PATTERN (insn)) != ASM_INPUT)
      {
	if (final_p)
	  {
	    extract_constrain_insn (insn);
	    continue;
	  }
	/* LRA code is based on assumption that all addresses can be
	   correctly decomposed.  LRA can generate reloads for
	   decomposable addresses.  The decomposition code checks the
	   correctness of the addresses.  So we don't need to check
	   the addresses here.  Don't call insn_invalid_p here, it can
	   change the code at this stage.  */
	if (recog_memoized (insn) < 0 && asm_noperands (PATTERN (insn)) < 0)
	  fatal_insn_not_found (insn);
      }
}

/* Determine if the current function has an exception receiver block
   that reaches the exit block via non-exceptional edges  */
static bool
has_nonexceptional_receiver (void)
{
  edge e;
  edge_iterator ei;
  basic_block *tos, *worklist, bb;

  /* If we're not optimizing, then just err on the safe side.  */
  if (!optimize)
    return true;

  /* First determine which blocks can reach exit via normal paths.  */
  tos = worklist = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun) + 1);

  FOR_EACH_BB_FN (bb, cfun)
    bb->flags &= ~BB_REACHABLE;

  /* Place the exit block on our worklist.  */
  EXIT_BLOCK_PTR_FOR_FN (cfun)->flags |= BB_REACHABLE;
  *tos++ = EXIT_BLOCK_PTR_FOR_FN (cfun);

  /* Iterate: find everything reachable from what we've already seen.  */
  while (tos != worklist)
    {
      bb = *--tos;

      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    free (worklist);
	    return true;
	  }
	else
	  {
	    basic_block src = e->src;

	    if (!(src->flags & BB_REACHABLE))
	      {
		src->flags |= BB_REACHABLE;
		*tos++ = src;
	      }
	  }
    }
  free (worklist);
  /* No exceptional block reached exit unexceptionally.	 */
  return false;
}


/* Process recursively X of INSN and add REG_INC notes if necessary.  */
static void
add_auto_inc_notes (rtx_insn *insn, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (code == MEM && auto_inc_p (XEXP (x, 0)))
    {
      add_reg_note (insn, REG_INC, XEXP (XEXP (x, 0), 0));
      return;
    }

  /* Scan all X sub-expressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	add_auto_inc_notes (insn, XEXP (x, i));
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  add_auto_inc_notes (insn, XVECEXP (x, i, j));
    }
}


/* Remove all REG_DEAD and REG_UNUSED notes and regenerate REG_INC.
   We change pseudos by hard registers without notification of DF and
   that can make the notes obsolete.  DF-infrastructure does not deal
   with REG_INC notes -- so we should regenerate them here.  */
static void
update_inc_notes (void)
{
  rtx *pnote;
  basic_block bb;
  rtx_insn *insn;

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      {
	pnote = &REG_NOTES (insn);
	while (*pnote != 0)
	  {
	    if (REG_NOTE_KIND (*pnote) == REG_DEAD
                || REG_NOTE_KIND (*pnote) == REG_UNUSED
                || REG_NOTE_KIND (*pnote) == REG_INC)
	      *pnote = XEXP (*pnote, 1);
	    else
	      pnote = &XEXP (*pnote, 1);
	  }

	if (AUTO_INC_DEC)
	  add_auto_inc_notes (insn, PATTERN (insn));
      }
}

/* Set to 1 while in lra.  */
int lra_in_progress;

/* Start of pseudo regnos before the LRA.  */
int lra_new_regno_start;

/* Start of reload pseudo regnos before the new spill pass.  */
int lra_constraint_new_regno_start;

/* Avoid spilling pseudos with regno more than the following value if
   it is possible.  */
int lra_bad_spill_regno_start;

/* Inheritance pseudo regnos before the new spill pass.	 */
bitmap_head lra_inheritance_pseudos;

/* Split regnos before the new spill pass.  */
bitmap_head lra_split_regs;

/* Reload pseudo regnos before the new assignment pass which still can
   be spilled after the assignment pass as memory is also accepted in
   insns for the reload pseudos.  */
bitmap_head lra_optional_reload_pseudos;

/* Pseudo regnos used for subreg reloads before the new assignment
   pass.  Such pseudos still can be spilled after the assignment
   pass.  */
bitmap_head lra_subreg_reload_pseudos;

/* File used for output of LRA debug information.  */
FILE *lra_dump_file;

/* True if we should try spill into registers of different classes
   instead of memory.  */
bool lra_reg_spill_p;

/* Set up value LRA_REG_SPILL_P.  */
static void
setup_reg_spill_flag (void)
{
  int cl, mode;

  if (targetm.spill_class != NULL)
    for (cl = 0; cl < (int) LIM_REG_CLASSES; cl++)
      for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
	if (targetm.spill_class ((enum reg_class) cl,
				 (machine_mode) mode) != NO_REGS)
	  {
	    lra_reg_spill_p = true;
	    return;
	  }
  lra_reg_spill_p = false;
}

/* True if the current function is too big to use regular algorithms
   in LRA. In other words, we should use simpler and faster algorithms
   in LRA.  It also means we should not worry about generation code
   for caller saves.  The value is set up in IRA.  */
bool lra_simple_p;

/* Major LRA entry function.  F is a file should be used to dump LRA
   debug info.  */
void
lra (FILE *f)
{
  int i;
  bool live_p, inserted_p;

  lra_dump_file = f;

  timevar_push (TV_LRA);

  /* Make sure that the last insn is a note.  Some subsequent passes
     need it.  */
  emit_note (NOTE_INSN_DELETED);

  COPY_HARD_REG_SET (lra_no_alloc_regs, ira_no_alloc_regs);

  init_reg_info ();
  expand_reg_info ();

  init_insn_recog_data ();

  /* Some quick check on RTL generated by previous passes.  */
  if (flag_checking)
    check_rtl (false);

  lra_in_progress = 1;

  lra_live_range_iter = lra_coalesce_iter = lra_constraint_iter = 0;
  lra_assignment_iter = lra_assignment_iter_after_spill = 0;
  lra_inheritance_iter = lra_undo_inheritance_iter = 0;
  lra_rematerialization_iter = 0;

  setup_reg_spill_flag ();

  /* Function remove_scratches can creates new pseudos for clobbers --
     so set up lra_constraint_new_regno_start before its call to
     permit changing reg classes for pseudos created by this
     simplification.  */
  lra_constraint_new_regno_start = lra_new_regno_start = max_reg_num ();
  lra_bad_spill_regno_start = INT_MAX;
  remove_scratches ();

  /* A function that has a non-local label that can reach the exit
     block via non-exceptional paths must save all call-saved
     registers.	 */
  if (cfun->has_nonlocal_label && has_nonexceptional_receiver ())
    crtl->saves_all_registers = 1;

  if (crtl->saves_all_registers)
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (! call_used_regs[i] && ! fixed_regs[i] && ! LOCAL_REGNO (i))
	df_set_regs_ever_live (i, true);

  /* We don't DF from now and avoid its using because it is to
     expensive when a lot of RTL changes are made.  */
  df_set_flags (DF_NO_INSN_RESCAN);
  lra_constraint_insn_stack.create (get_max_uid ());
  lra_constraint_insn_stack_bitmap = sbitmap_alloc (get_max_uid ());
  bitmap_clear (lra_constraint_insn_stack_bitmap);
  lra_live_ranges_init ();
  lra_constraints_init ();
  lra_curr_reload_num = 0;
  push_insns (get_last_insn (), NULL);
  /* It is needed for the 1st coalescing.  */
  bitmap_initialize (&lra_inheritance_pseudos, &reg_obstack);
  bitmap_initialize (&lra_split_regs, &reg_obstack);
  bitmap_initialize (&lra_optional_reload_pseudos, &reg_obstack);
  bitmap_initialize (&lra_subreg_reload_pseudos, &reg_obstack);
  live_p = false;
  if (get_frame_size () != 0 && crtl->stack_alignment_needed)
    /* If we have a stack frame, we must align it now.  The stack size
       may be a part of the offset computation for register
       elimination.  */
    assign_stack_local (BLKmode, 0, crtl->stack_alignment_needed);
  lra_init_equiv ();
  for (;;)
    {
      for (;;)
	{
	  bool reloads_p = lra_constraints (lra_constraint_iter == 0);
	  /* Constraint transformations may result in that eliminable
	     hard regs become uneliminable and pseudos which use them
	     should be spilled.	 It is better to do it before pseudo
	     assignments.

	     For example, rs6000 can make
	     RS6000_PIC_OFFSET_TABLE_REGNUM uneliminable if we started
	     to use a constant pool.  */
	  lra_eliminate (false, false);
	  /* We should try to assign hard registers to scratches even
	     if there were no RTL transformations in lra_constraints.
	     Also we should check IRA assignments on the first
	     iteration as they can be wrong because of early clobbers
	     operands which are ignored in IRA.  */
	  if (! reloads_p && lra_constraint_iter > 1)
	    {
	      /* Stack is not empty here only when there are changes
		 during the elimination sub-pass.  */
	      if (bitmap_empty_p (lra_constraint_insn_stack_bitmap))
		break;
	      else
		/* If there are no reloads but changing due
		   elimination, restart the constraint sub-pass
		   first.  */
		continue;
	    }
	  /* Do inheritance only for regular algorithms.  */
	  if (! lra_simple_p)
	    {
	      if (flag_ipa_ra)
		{
		  if (live_p)
		    lra_clear_live_ranges ();
		  /* As a side-effect of lra_create_live_ranges, we calculate
		     actual_call_used_reg_set,  which is needed during
		     lra_inheritance.  */
		  lra_create_live_ranges (true, true);
		  live_p = true;
		}
	      lra_inheritance ();
	    }
	  if (live_p)
	    lra_clear_live_ranges ();
	  /* We need live ranges for lra_assign -- so build them.  But
	     don't remove dead insns or change global live info as we
	     can undo inheritance transformations after inheritance
	     pseudo assigning.  */
	  lra_create_live_ranges (true, false);
	  live_p = true;
	  /* If we don't spill non-reload and non-inheritance pseudos,
	     there is no sense to run memory-memory move coalescing.
	     If inheritance pseudos were spilled, the memory-memory
	     moves involving them will be removed by pass undoing
	     inheritance.  */
	  if (lra_simple_p)
	    lra_assign ();
	  else
	    {
	      bool spill_p = !lra_assign ();

	      if (lra_undo_inheritance ())
		live_p = false;
	      if (spill_p)
		{
		  if (! live_p)
		    {
		      lra_create_live_ranges (true, true);
		      live_p = true;
		    }
		  if (lra_coalesce ())
		    live_p = false;
		}
	      if (! live_p)
		lra_clear_live_ranges ();
	    }
	}
      /* Don't clear optional reloads bitmap until all constraints are
	 satisfied as we need to differ them from regular reloads.  */
      bitmap_clear (&lra_optional_reload_pseudos);
      bitmap_clear (&lra_subreg_reload_pseudos);
      bitmap_clear (&lra_inheritance_pseudos);
      bitmap_clear (&lra_split_regs);
      if (! live_p)
	{
	  /* We need full live info for spilling pseudos into
	     registers instead of memory.  */
	  lra_create_live_ranges (lra_reg_spill_p, true);
	  live_p = true;
	}
      /* We should check necessity for spilling here as the above live
	 range pass can remove spilled pseudos.  */
      if (! lra_need_for_spills_p ())
	break;
      /* Now we know what pseudos should be spilled.  Try to
	 rematerialize them first.  */
      if (lra_remat ())
	{
	  /* We need full live info -- see the comment above.  */
	  lra_create_live_ranges (lra_reg_spill_p, true);
	  live_p = true;
	  if (! lra_need_for_spills_p ())
	    break;
	}
      lra_spill ();
      /* Assignment of stack slots changes elimination offsets for
	 some eliminations.  So update the offsets here.  */
      lra_eliminate (false, false);
      lra_constraint_new_regno_start = max_reg_num ();
      if (lra_bad_spill_regno_start == INT_MAX
	  && lra_inheritance_iter > LRA_MAX_INHERITANCE_PASSES
	  && lra_rematerialization_iter > LRA_MAX_REMATERIALIZATION_PASSES)
	/* After switching off inheritance and rematerialization
	   passes, avoid spilling reload pseudos will be created to
	   prevent LRA cycling in some complicated cases.  */
	lra_bad_spill_regno_start = lra_constraint_new_regno_start;
      lra_assignment_iter_after_spill = 0;
    }
  restore_scratches ();
  lra_eliminate (true, false);
  lra_final_code_change ();
  lra_in_progress = 0;
  if (live_p)
    lra_clear_live_ranges ();
  lra_live_ranges_finish ();
  lra_constraints_finish ();
  finish_reg_info ();
  sbitmap_free (lra_constraint_insn_stack_bitmap);
  lra_constraint_insn_stack.release ();
  finish_insn_recog_data ();
  regstat_free_n_sets_and_refs ();
  regstat_free_ri ();
  reload_completed = 1;
  update_inc_notes ();

  inserted_p = fixup_abnormal_edges ();

  /* We've possibly turned single trapping insn into multiple ones.  */
  if (cfun->can_throw_non_call_exceptions)
    {
      auto_sbitmap blocks (last_basic_block_for_fn (cfun));
      bitmap_ones (blocks);
      find_many_sub_basic_blocks (blocks);
    }

  if (inserted_p)
    commit_edge_insertions ();

  /* Replacing pseudos with their memory equivalents might have
     created shared rtx.  Subsequent passes would get confused
     by this, so unshare everything here.  */
  unshare_all_rtl_again (get_insns ());

  if (flag_checking)
    check_rtl (true);

  timevar_pop (TV_LRA);
}

/* Called once per compiler to initialize LRA data once.  */
void
lra_init_once (void)
{
  init_insn_code_data_once ();
}

/* Called once per compiler to finish LRA data which are initialize
   once.  */
void
lra_finish_once (void)
{
  finish_insn_code_data_once ();
}
