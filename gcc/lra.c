/* LRA (local register allocator) driver and LRA utilities.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
         ----------------    No    ------------  pseudos   -------------------
        | Spilled pseudo | change |Constraints:| or insns | Inheritance/split |
        |    to memory   |<-------|    RTL     |--------->|  transformations  |
        |  substitution  |        | transfor-  |          |    in EBB scope   |
         ----------------         |  mations   |           -------------------
                |                   ------------
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
#include "tm.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "recog.h"
#include "output.h"
#include "addresses.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "except.h"
#include "tree-pass.h"
#include "timevar.h"
#include "target.h"
#include "vec.h"
#include "ira.h"
#include "lra-int.h"
#include "df.h"

/* Hard registers currently not available for allocation.  It can
   changed after some hard  registers become not eliminable.  */
HARD_REG_SET lra_no_alloc_regs;

static int get_new_reg_value (void);
static void expand_reg_info (void);
static void invalidate_insn_recog_data (int);
static int get_insn_freq (rtx);
static void invalidate_insn_data_regno_info (lra_insn_recog_data_t, rtx, int);

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
lra_create_new_reg_with_unique_value (enum machine_mode md_mode, rtx original,
				      enum reg_class rclass, const char *title)
{
  enum machine_mode mode;
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
lra_create_new_reg (enum machine_mode md_mode, rtx original,
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
lra_invalidate_insn_data (rtx insn)
{
  lra_invalidate_insn_regno_info (insn);
  invalidate_insn_recog_data (INSN_UID (insn));
}

/* Mark INSN deleted and invalidate the insn related info used by
   LRA.	 */
void
lra_set_insn_deleted (rtx insn)
{
  lra_invalidate_insn_data (insn);
  SET_INSN_DELETED (insn);
}

/* Delete an unneeded INSN and any previous insns who sole purpose is
   loading data that is dead in INSN.  */
void
lra_delete_dead_insn (rtx insn)
{
  rtx prev = prev_real_insn (insn);
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
static rtx
emit_add3_insn (rtx x, rtx y, rtx z)
{
  rtx insn, last;

  last = get_last_insn ();
  insn = emit_insn (gen_rtx_SET (VOIDmode, x,
				 gen_rtx_PLUS (GET_MODE (y), y, z)));
  if (recog_memoized (insn) < 0)
    {
      delete_insns_since (last);
      insn = NULL_RTX;
    }
  return insn;
}

/* Emit insn x = x + y.  Return the insn.  We use gen_add2_insn as the
   last resort.  */
static rtx
emit_add2_insn (rtx x, rtx y)
{
  rtx insn;

  insn = emit_add3_insn (x, x, y);
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
  rtx insn, last;
  rtx a1, a2, base, index, disp, scale, index_scale;
  bool ok_p;

  insn = emit_add3_insn (x, y, z);
  old = max_reg_num ();
  if (insn != NULL_RTX)
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
      if (! REG_P (base)
	  || (index != NULL_RTX && ! REG_P (index))
	  || (disp != NULL_RTX && ! CONSTANT_P (disp))
	  || (scale != NULL_RTX && ! CONSTANT_P (scale)))
	{
	  /* Probably we have no 3 op add.  Last chance is to use 2-op
	     add insn.  To succeed, don't move Z to X as an address
	     segment always comes in Y.  Otherwise, we might fail when
	     adding the address segment to register.  */
	  lra_assert (x != y && x != z);
	  emit_move_insn (x, y);
	  insn = emit_add2_insn (x, z);
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
	      insn = emit_add2_insn (x, base);
	      lra_assert (insn != NULL_RTX);
	    }
	  else if (scale == NULL_RTX)
	    {
	      /* Try x = base + disp.  */
	      lra_assert (base != NULL_RTX);
	      last = get_last_insn ();
	      insn = emit_move_insn (x, gen_rtx_PLUS (GET_MODE (base),
						      base, disp));
	      if (recog_memoized (insn) < 0)
		{
		  delete_insns_since (last);
		  /* Generate x = disp; x = x + base.  */
		  emit_move_insn (x, disp);
		  insn = emit_add2_insn (x, base);
		  lra_assert (insn != NULL_RTX);
		}
	      /* Generate x = x + index.  */
	      if (index != NULL_RTX)
		{
		  insn = emit_add2_insn (x, index);
		  lra_assert (insn != NULL_RTX);
		}
	    }
	  else
	    {
	      /* Try x = index_scale; x = x + disp; x = x + base.  */
	      last = get_last_insn ();
	      insn = emit_move_insn (x, index_scale);
	      ok_p = false;
	      if (recog_memoized (insn) >= 0)
		{
		  insn = emit_add2_insn (x, disp);
		  if (insn != NULL_RTX)
		    {
		      insn = emit_add2_insn (x, disp);
		      if (insn != NULL_RTX)
			ok_p = true;
		    }
		}
	      if (! ok_p)
		{
		  delete_insns_since (last);
		  /* Generate x = disp; x = x + base; x = x + index_scale.  */
		  emit_move_insn (x, disp);
		  insn = emit_add2_insn (x, base);
		  lra_assert (insn != NULL_RTX);
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
static alloc_pool insn_reg_pool;

/* Initiate pool for insn reg info.  */
static void
init_insn_regs (void)
{
  insn_reg_pool
    = create_alloc_pool ("insn regs", sizeof (struct lra_insn_reg), 100);
}

/* Create LRA insn related info about a reference to REGNO in INSN with
   TYPE (in/out/inout), biggest reference mode MODE, flag that it is
   reference through subreg (SUBREG_P), flag that is early clobbered
   in the insn (EARLY_CLOBBER), and reference to the next insn reg
   info (NEXT).	 */
static struct lra_insn_reg *
new_insn_reg (rtx insn, int regno, enum op_type type, enum machine_mode mode,
	      bool subreg_p, bool early_clobber, struct lra_insn_reg *next)
{
  struct lra_insn_reg *ir;

  ir = (struct lra_insn_reg *) pool_alloc (insn_reg_pool);
  ir->type = type;
  ir->biggest_mode = mode;
  if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (lra_reg_info[regno].biggest_mode)
      && NONDEBUG_INSN_P (insn))
    lra_reg_info[regno].biggest_mode = mode;
  ir->subreg_p = subreg_p;
  ir->early_clobber = early_clobber;
  ir->regno = regno;
  ir->next = next;
  return ir;
}

/* Free insn reg info IR.  */
static void
free_insn_reg (struct lra_insn_reg *ir)
{
  pool_free (insn_reg_pool, ir);
}

/* Free insn reg info list IR.	*/
static void
free_insn_regs (struct lra_insn_reg *ir)
{
  struct lra_insn_reg *next_ir;

  for (; ir != NULL; ir = next_ir)
    {
      next_ir = ir->next;
      free_insn_reg (ir);
    }
}

/* Finish pool for insn reg info.  */
static void
finish_insn_regs (void)
{
  free_alloc_pool (insn_reg_pool);
}



/* This page contains code dealing LRA insn info (or in other words
   LRA internal insn representation).  */

struct target_lra_int default_target_lra_int;
#if SWITCHABLE_TARGET
struct target_lra_int *this_target_lra_int = &default_target_lra_int;
#endif

/* Map INSN_CODE -> the static insn data.  This info is valid during
   all translation unit.  */
struct lra_static_insn_data *insn_code_data[LAST_INSN_CODE];

/* Debug insns are represented as a special insn with one input
   operand which is RTL expression in var_location.  */

/* The following data are used as static insn operand data for all
   debug insns.	 If structure lra_operand_data is changed, the
   initializer should be changed too.  */
static struct lra_operand_data debug_operand_data =
  {
    NULL, /* alternative  */
    VOIDmode, /* We are not interesting in the operand mode.  */
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
  memset (op_alt_data, 0, sizeof (op_alt_data));
}

/* Called once per compiler work to finalize some LRA data related to
   insns.  */
static void
finish_insn_code_data_once (void)
{
  int i;

  for (i = 0; i < LAST_INSN_CODE; i++)
    {
      if (insn_code_data[i] != NULL)
	free (insn_code_data[i]);
      if (op_alt_data[i] != NULL)
	free (op_alt_data[i]);
    }
}

/* Initialize LRA info about operands in insn alternatives.  */
static void
init_op_alt_data (void)
{
 int i;

  for (i = 0; i < LAST_INSN_CODE; i++)
    if (op_alt_data[i] != NULL)
      {
	free (op_alt_data[i]);
	op_alt_data[i] = NULL;
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

  lra_assert (icode < LAST_INSN_CODE);
  if (icode >= 0 && (data = insn_code_data[icode]) != NULL)
    return data;
  lra_assert (nop >= 0 && ndup >= 0 && nalt >= 0);
  n_bytes = sizeof (struct lra_static_insn_data)
	    + sizeof (struct lra_operand_data) * nop
	    + sizeof (int) * ndup;
  data = XNEWVAR (struct lra_static_insn_data, n_bytes);
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
  init_insn_regs ();
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
  if (HAVE_ATTR_enabled && data->alternative_enabled_p != NULL)
    free (data->alternative_enabled_p);
  if (data->icode < 0 && NONDEBUG_INSN_P (data->insn))
    {
      if (data->insn_static_data->operand_alternative != NULL)
	free (data->insn_static_data->operand_alternative);
      free_insn_regs (data->insn_static_data->hard_regs);
      free (data->insn_static_data);
    }
  free_insn_regs (data->regs);
  data->regs = NULL;
  free (data);
}

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
  free (lra_insn_recog_data);
}

/* Setup info about operands in alternatives of LRA DATA of insn.  */
static void
setup_operand_alternative (lra_insn_recog_data_t data)
{
  int i, nop, nalt;
  int icode = data->icode;
  struct lra_static_insn_data *static_data = data->insn_static_data;

  if (icode >= 0
      && (static_data->operand_alternative = op_alt_data[icode]) != NULL)
    return;
  static_data->commutative = -1;
  nop = static_data->n_operands;
  if (nop == 0)
    {
      static_data->operand_alternative = NULL;
      return;
    }
  nalt = static_data->n_alternatives;
  static_data->operand_alternative = XNEWVEC (struct operand_alternative,
					      nalt * nop);
  memset (static_data->operand_alternative, 0,
	  nalt * nop * sizeof (struct operand_alternative));
  if (icode >= 0)
    op_alt_data[icode] = static_data->operand_alternative;
  for (i = 0; i < nop; i++)
    {
      int j;
      struct operand_alternative *op_alt_start, *op_alt;
      const char *p = static_data->operand[i].constraint;

      static_data->operand[i].early_clobber = 0;
      op_alt_start = &static_data->operand_alternative[i];

      for (j = 0; j < nalt; j++)
	{
	  op_alt = op_alt_start + j * nop;
	  op_alt->cl = NO_REGS;
	  op_alt->constraint = p;
	  op_alt->matches = -1;
	  op_alt->matched = -1;

	  if (*p == '\0' || *p == ',')
	    {
	      op_alt->anything_ok = 1;
	      continue;
	    }

	  for (;;)
	    {
	      char c = *p;
	      if (c == '#')
		do
		  c = *++p;
		while (c != ',' && c != '\0');
	      if (c == ',' || c == '\0')
		{
		  p++;
		  break;
		}

	      switch (c)
		{
		case '=': case '+': case '*':
		case 'E': case 'F': case 'G': case 'H':
		case 's': case 'i': case 'n':
		case 'I': case 'J': case 'K': case 'L':
		case 'M': case 'N': case 'O': case 'P':
		  /* These don't say anything we care about.  */
		  break;

		case '%':
		  /* We currently only support one commutative pair of
		     operands.	*/
		  if (static_data->commutative < 0)
		    static_data->commutative = i;
		  else
		    lra_assert (data->icode < 0); /* Asm  */

		  /* The last operand should not be marked
		     commutative.  */
		  lra_assert (i != nop - 1);
		  break;

		case '?':
		  op_alt->reject += LRA_LOSER_COST_FACTOR;
		  break;
		case '!':
		  op_alt->reject += LRA_MAX_REJECT;
		  break;
		case '&':
		  op_alt->earlyclobber = 1;
		  static_data->operand[i].early_clobber = 1;
		  break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		  {
		    char *end;
		    op_alt->matches = strtoul (p, &end, 10);
		    static_data->operand_alternative
		      [j * nop + op_alt->matches].matched = i;
		    p = end;
		  }
		  continue;

		case TARGET_MEM_CONSTRAINT:
		  op_alt->memory_ok = 1;
		  break;
		case '<':
		  op_alt->decmem_ok = 1;
		  break;
		case '>':
		  op_alt->incmem_ok = 1;
		  break;
		case 'V':
		  op_alt->nonoffmem_ok = 1;
		  break;
		case 'o':
		  op_alt->offmem_ok = 1;
		  break;
		case 'X':
		  op_alt->anything_ok = 1;
		  break;

		case 'p':
		  static_data->operand[i].is_address = true;
		  op_alt->is_address = 1;
		  op_alt->cl = (reg_class_subunion[(int) op_alt->cl]
				[(int) base_reg_class (VOIDmode,
						       ADDR_SPACE_GENERIC,
						       ADDRESS, SCRATCH)]);
		  break;

		case 'g':
		case 'r':
		  op_alt->cl =
		   reg_class_subunion[(int) op_alt->cl][(int) GENERAL_REGS];
		  break;

		default:
		  if (EXTRA_MEMORY_CONSTRAINT (c, p))
		    {
		      op_alt->memory_ok = 1;
		      break;
		    }
		  if (EXTRA_ADDRESS_CONSTRAINT (c, p))
		    {
		      static_data->operand[i].is_address = true;
		      op_alt->is_address = 1;
		      op_alt->cl
			= (reg_class_subunion
			   [(int) op_alt->cl]
			   [(int) base_reg_class (VOIDmode, ADDR_SPACE_GENERIC,
						  ADDRESS, SCRATCH)]);
		      break;
		    }

		  op_alt->cl
		    = (reg_class_subunion
		       [(int) op_alt->cl]
		       [(int)
			REG_CLASS_FROM_CONSTRAINT ((unsigned char) c, p)]);
		  break;
		}
	      p += CONSTRAINT_LEN (c, p);
	    }
	}
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
  enum machine_mode mode;
  struct lra_insn_reg *curr;
  rtx op = *x;
  enum rtx_code code = GET_CODE (op);
  const char *fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < data->insn_static_data->n_operands; i++)
    if (x == data->operand_loc[i])
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
      for (last = regno + hard_regno_nregs[regno][mode];
	   regno < last;
	   regno++)
	if (! TEST_HARD_REG_BIT (lra_no_alloc_regs, regno)
	    || TEST_HARD_REG_BIT (eliminable_regset, regno))
	  {
	    for (curr = list; curr != NULL; curr = curr->next)
	      if (curr->regno == regno && curr->subreg_p == subreg_p
		  && curr->biggest_mode == mode)
		{
		  if (curr->type != type)
		    curr->type = OP_INOUT;
		  if (curr->early_clobber != early_clobber)
		    curr->early_clobber = true;
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
				     early_clobber, list);
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
lra_set_insn_recog_data (rtx insn)
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
      data->alternative_enabled_p = NULL;
      data->operand_loc = XNEWVEC (rtx *, 1);
      data->operand_loc[0] = &INSN_VAR_LOCATION_LOC (insn);
      return data;
    }
  if (icode < 0)
    {
      int nop;
      enum machine_mode operand_mode[MAX_RECOG_OPERANDS];
      const char *constraints[MAX_RECOG_OPERANDS];

      nop = asm_noperands (PATTERN (insn));
      data->operand_loc = data->dup_loc = NULL;
      if (nop < 0)
	/* Its is a special insn like USE or CLOBBER.  */
	data->insn_static_data = insn_static_data
	  = get_static_insn_data (-1, 0, 0, 1);
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
	  n = 1;
	  if (nop > 0)
	    {
	      const char *p =  recog_data.constraints[0];

	      for (p =	constraints[0]; *p; p++)
		n += *p == ',';
	    }
	  data->insn_static_data = insn_static_data
	    = get_static_insn_data (-1, nop, 0, n);
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
      data->alternative_enabled_p = NULL;
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
      if (HAVE_ATTR_enabled)
	{
	  bool *bp;

	  n = insn_static_data->n_alternatives;
	  lra_assert (n >= 0);
	  data->alternative_enabled_p = bp = XNEWVEC (bool, n);
	  /* Cache the insn because we don't want to call extract_insn
	     from get_attr_enabled as extract_insn modifies
	     which_alternative.  The attribute enabled should not depend
	     on insn operands, operand modes, operand types, and operand
	     constraints.  It should depend on the architecture.  If it
	     is not true, we should rewrite this file code to use
	     extract_insn instead of less expensive insn_extract.  */
	  recog_data.insn = insn;
	  for (i = 0; i < n; i++)
	    {
	      which_alternative = i;
	      bp[i] = get_attr_enabled (insn);
	    }
	}
    }
  if (GET_CODE (PATTERN (insn)) == CLOBBER || GET_CODE (PATTERN (insn)) == USE)
    insn_static_data->hard_regs = NULL;
  else
    insn_static_data->hard_regs
      = collect_non_operand_hard_regs (&PATTERN (insn), data,
				       NULL, OP_IN, false);
  setup_operand_alternative (data);
  data->arg_hard_regs = NULL;
  if (CALL_P (insn))
    {
      rtx link;
      int n_hard_regs, regno, arg_hard_regs[FIRST_PSEUDO_REGISTER];

      n_hard_regs = 0;
      /* Finding implicit hard register usage.	We believe it will be
	 not changed whatever transformations are used.	 Call insns
	 are such example.  */
      for (link = CALL_INSN_FUNCTION_USAGE (insn);
	   link != NULL_RTX;
	   link = XEXP (link, 1))
	if (GET_CODE (XEXP (link, 0)) == USE
	    && REG_P (XEXP (XEXP (link, 0), 0)))
	  {
	    regno = REGNO (XEXP (XEXP (link, 0), 0));
	    lra_assert (regno < FIRST_PSEUDO_REGISTER);
	    /* It is an argument register.  */
	    for (i = (hard_regno_nregs
		      [regno][GET_MODE (XEXP (XEXP (link, 0), 0))]) - 1;
		 i >= 0;
		 i--)
	      arg_hard_regs[n_hard_regs++] = regno + i;
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
lra_update_insn_recog_data (rtx insn)
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
      enum machine_mode operand_mode[MAX_RECOG_OPERANDS];
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
#ifdef ENABLE_CHECKING
	  {
	    int i;

	    for (i = 0; i < nop; i++)
	      lra_assert
		(insn_static_data->operand[i].mode == operand_mode[i]
		 && insn_static_data->operand[i].constraint == constraints[i]
		 && ! insn_static_data->operand[i].is_operator);
	  }
#endif
	}
#ifdef ENABLE_CHECKING
      {
	int i;

	for (i = 0; i < insn_static_data->n_operands; i++)
	  lra_assert
	    (insn_static_data->operand[i].type
	     == (insn_static_data->operand[i].constraint[0] == '=' ? OP_OUT
		 : insn_static_data->operand[i].constraint[0] == '+' ? OP_INOUT
		 : OP_IN));
      }
#endif
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
#if HAVE_ATTR_enabled
#ifdef ENABLE_CHECKING
      {
	int i;
	bool *bp;

	n = insn_static_data->n_alternatives;
	bp = data->alternative_enabled_p;
	lra_assert (n >= 0 && bp != NULL);
	/* Cache the insn to prevent extract_insn call from
	   get_attr_enabled.  */
	recog_data.insn = insn;
	for (i = 0; i < n; i++)
	  {
	    which_alternative = i;
	    lra_assert (bp[i] == get_attr_enabled (insn));
	  }
      }
#endif
#endif
    }
  return data;
}

/* Set up that INSN is using alternative ALT now.  */
void
lra_set_used_insn_alternative (rtx insn, int alt)
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

/* Pools for copies.  */
static alloc_pool copy_pool;

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
  lra_reg_info[i].preferred_hard_regno1 = -1;
  lra_reg_info[i].preferred_hard_regno2 = -1;
  lra_reg_info[i].preferred_hard_regno_profit1 = 0;
  lra_reg_info[i].preferred_hard_regno_profit2 = 0;
  lra_reg_info[i].biggest_mode = VOIDmode;
  lra_reg_info[i].live_ranges = NULL;
  lra_reg_info[i].nrefs = lra_reg_info[i].freq = 0;
  lra_reg_info[i].last_reload = 0;
  lra_reg_info[i].restore_regno = -1;
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
  copy_pool
    = create_alloc_pool ("lra copies", sizeof (struct lra_copy), 100);
  copy_vec.create (100);
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
  free_alloc_pool (copy_pool);
  copy_vec.release ();
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
      pool_free (copy_pool, cp);
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
      int temp = regno2;

      regno1_dest_p = false;
      regno2 = regno1;
      regno1 = temp;
    }
  cp = (lra_copy_t) pool_alloc (copy_pool);
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
   about registers in X to the insn DATA.  */
static void
add_regs_to_insn_regno_info (lra_insn_recog_data_t data, rtx x, int uid,
			     enum op_type type, bool early_clobber)
{
  int i, j, regno;
  bool subreg_p;
  enum machine_mode mode;
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
      if (regno < FIRST_PSEUDO_REGISTER
	  && TEST_HARD_REG_BIT (lra_no_alloc_regs, regno)
	  && ! TEST_HARD_REG_BIT (eliminable_regset, regno))
	return;
      expand_reg_info ();
      if (bitmap_set_bit (&lra_reg_info[regno].insn_bitmap, uid))
	{
	  data->regs = new_insn_reg (data->insn, regno, type, mode, subreg_p,
				     early_clobber, data->regs);
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
					     data->regs);
		else
		  {
		    if (curr->type != type)
		      curr->type = OP_INOUT;
		    if (curr->early_clobber != early_clobber)
		      curr->early_clobber = true;
		  }
		return;
	      }
	  gcc_unreachable ();
	}
    }

  switch (code)
    {
    case SET:
      add_regs_to_insn_regno_info (data, SET_DEST (x), uid, OP_OUT, false);
      add_regs_to_insn_regno_info (data, SET_SRC (x), uid, OP_IN, false);
      break;
    case CLOBBER:
      /* We treat clobber of non-operand hard registers as early
	 clobber (the behavior is expected from asm).  */
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_OUT, true);
      break;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC:
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_INOUT, false);
      break;
    case PRE_MODIFY: case POST_MODIFY:
      add_regs_to_insn_regno_info (data, XEXP (x, 0), uid, OP_INOUT, false);
      add_regs_to_insn_regno_info (data, XEXP (x, 1), uid, OP_IN, false);
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
	    add_regs_to_insn_regno_info (data, XEXP (x, i), uid, type, false);
	  else if (fmt[i] == 'E')
	    {
	      for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		add_regs_to_insn_regno_info (data, XVECEXP (x, i, j), uid,
					     type, false);
	    }
	}
    }
}

/* Return execution frequency of INSN.	*/
static int
get_insn_freq (rtx insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);

  gcc_checking_assert (bb != NULL);
  return REG_FREQ_FROM_BB (bb);
}

/* Invalidate all reg info of INSN with DATA and execution frequency
   FREQ.  Update common info about the invalidated registers.  */
static void
invalidate_insn_data_regno_info (lra_insn_recog_data_t data, rtx insn,
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
      free_insn_reg (ir);
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
lra_invalidate_insn_regno_info (rtx insn)
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
lra_update_insn_regno_info (rtx insn)
{
  int i, uid, freq;
  lra_insn_recog_data_t data;
  struct lra_static_insn_data *static_data;
  enum rtx_code code;

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
				 static_data->operand[i].early_clobber);
  if ((code = GET_CODE (PATTERN (insn))) == CLOBBER || code == USE)
    add_regs_to_insn_regno_info (data, XEXP (PATTERN (insn), 0), uid,
				 code == USE ? OP_IN : OP_OUT, false);
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



/* This page contains code dealing with stack of the insns which
   should be processed by the next constraint pass.  */

/* Bitmap used to put an insn on the stack only in one exemplar.  */
static sbitmap lra_constraint_insn_stack_bitmap;

/* The stack itself.  */
vec<rtx> lra_constraint_insn_stack;

/* Put INSN on the stack.  If ALWAYS_UPDATE is true, always update the reg
   info for INSN, otherwise only update it if INSN is not already on the
   stack.  */
static inline void
lra_push_insn_1 (rtx insn, bool always_update)
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
lra_push_insn (rtx insn)
{
  lra_push_insn_1 (insn, false);
}

/* Put INSN on the stack and update its reg info.  */
void
lra_push_insn_and_update_insn_regno_info (rtx insn)
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
rtx
lra_pop_insn (void)
{
  rtx insn = lra_constraint_insn_stack.pop ();
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
push_insns (rtx from, rtx to)
{
  rtx insn;

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
setup_sp_offset (rtx from, rtx last)
{
  rtx before = next_nonnote_insn_bb (last);
  HOST_WIDE_INT offset = (before == NULL_RTX || ! INSN_P (before)
			  ? 0 : lra_get_insn_recog_data (before)->sp_offset);

  for (rtx insn = from; insn != NEXT_INSN (last); insn = NEXT_INSN (insn))
    lra_get_insn_recog_data (insn)->sp_offset = offset;
}

/* Emit insns BEFORE before INSN and insns AFTER after INSN.  Put the
   insns onto the stack.  Print about emitting the insns with
   TITLE.  */
void
lra_process_new_insns (rtx insn, rtx before, rtx after, const char *title)
{
  rtx last;

  if (before == NULL_RTX && after == NULL_RTX)
    return;
  if (lra_dump_file != NULL)
    {
      dump_insn_slim (lra_dump_file, insn);
      if (before != NULL_RTX)
	{
	  fprintf (lra_dump_file,"    %s before:\n", title);
	  dump_rtl_slim (lra_dump_file, before, NULL_RTX, -1, 0);
	}
      if (after != NULL_RTX)
	{
	  fprintf (lra_dump_file, "    %s after:\n", title);
	  dump_rtl_slim (lra_dump_file, after, NULL_RTX, -1, 0);
	}
      fprintf (lra_dump_file, "\n");
    }
  if (before != NULL_RTX)
    {
      emit_insn_before (before, insn);
      push_insns (PREV_INSN (insn), PREV_INSN (before));
      setup_sp_offset (before, PREV_INSN (insn));
    }
  if (after != NULL_RTX)
    {
      for (last = after; NEXT_INSN (last) != NULL_RTX; last = NEXT_INSN (last))
	;
      emit_insn_after (after, insn);
      push_insns (last, insn);
      setup_sp_offset (after, last);
    }
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
  rtx insn; /* Insn where the scratch was.  */
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
lra_former_scratch_operand_p (rtx insn, int nop)
{
  return bitmap_bit_p (&scratch_operand_bitmap,
		       INSN_UID (insn) * MAX_RECOG_OPERANDS + nop) != 0;
}

/* Change scratches onto pseudos and save their location.  */
static void
remove_scratches (void)
{
  int i;
  bool insn_changed_p;
  basic_block bb;
  rtx insn, reg;
  sloc_t loc;
  lra_insn_recog_data_t id;
  struct lra_static_insn_data *static_id;

  scratches.create (get_max_uid ());
  bitmap_initialize (&scratch_bitmap, &reg_obstack);
  bitmap_initialize (&scratch_operand_bitmap, &reg_obstack);
  FOR_EACH_BB (bb)
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
	      add_reg_note (insn, REG_UNUSED, reg);
	      lra_update_dup (id, i);
	      loc = XNEW (struct sloc);
	      loc->insn = insn;
	      loc->nop = i;
	      scratches.safe_push (loc);
	      bitmap_set_bit (&scratch_bitmap, REGNO (*id->operand_loc[i]));
	      bitmap_set_bit (&scratch_operand_bitmap,
			      INSN_UID (insn) * MAX_RECOG_OPERANDS + i);
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
  rtx last = NULL_RTX;
  lra_insn_recog_data_t id = NULL;

  for (i = 0; scratches.iterate (i, &loc); i++)
    {
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



#ifdef ENABLE_CHECKING

/* Function checks RTL for correctness.	 If FINAL_P is true, it is
   done at the end of LRA and the check is more rigorous.  */
static void
check_rtl (bool final_p)
{
  basic_block bb;
  rtx insn;

  lra_assert (! final_p || reload_completed);
  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn)
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER
	&& GET_CODE (PATTERN (insn)) != ASM_INPUT)
      {
	if (final_p)
	  {
	    extract_insn (insn);
	    lra_assert (constrain_operands (1));
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
#endif /* #ifdef ENABLE_CHECKING */

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

  FOR_EACH_BB (bb)
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

#ifdef AUTO_INC_DEC

/* Process recursively X of INSN and add REG_INC notes if necessary.  */
static void
add_auto_inc_notes (rtx insn, rtx x)
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

#endif

/* Remove all REG_DEAD and REG_UNUSED notes and regenerate REG_INC.
   We change pseudos by hard registers without notification of DF and
   that can make the notes obsolete.  DF-infrastructure does not deal
   with REG_INC notes -- so we should regenerate them here.  */
static void
update_inc_notes (void)
{
  rtx *pnote;
  basic_block bb;
  rtx insn;

  FOR_EACH_BB (bb)
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
#ifdef AUTO_INC_DEC
	add_auto_inc_notes (insn, PATTERN (insn));
#endif
      }
}

/* Set to 1 while in lra.  */
int lra_in_progress;

/* Start of pseudo regnos before the LRA.  */
int lra_new_regno_start;

/* Start of reload pseudo regnos before the new spill pass.  */
int lra_constraint_new_regno_start;

/* Inheritance pseudo regnos before the new spill pass.	 */
bitmap_head lra_inheritance_pseudos;

/* Split regnos before the new spill pass.  */
bitmap_head lra_split_regs;

/* Reload pseudo regnos before the new assignmnet pass which still can
   be spilled after the assinment pass as memory is also accepted in
   insns for the reload pseudos.  */
bitmap_head lra_optional_reload_pseudos;

/* Pseudo regnos used for subreg reloads before the new assignment
   pass.  Such pseudos still can be spilled after the assinment
   pass.  */
bitmap_head lra_subreg_reload_pseudos;

/* First UID of insns generated before a new spill pass.  */
int lra_constraint_new_insn_uid_start;

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
				 (enum machine_mode) mode) != NO_REGS)
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
  bool live_p, scratch_p, inserted_p;

  lra_dump_file = f;

  timevar_push (TV_LRA);

  /* Make sure that the last insn is a note.  Some subsequent passes
     need it.  */
  emit_note (NOTE_INSN_DELETED);

  COPY_HARD_REG_SET (lra_no_alloc_regs, ira_no_alloc_regs);

  init_reg_info ();
  expand_reg_info ();

  init_insn_recog_data ();

#ifdef ENABLE_CHECKING
  /* Some quick check on RTL generated by previous passes.  */
  check_rtl (false);
#endif

  lra_in_progress = 1;

  lra_live_range_iter = lra_coalesce_iter = 0;
  lra_constraint_iter = lra_constraint_iter_after_spill = 0;
  lra_inheritance_iter = lra_undo_inheritance_iter = 0;

  setup_reg_spill_flag ();

  /* Function remove_scratches can creates new pseudos for clobbers --
     so set up lra_constraint_new_regno_start before its call to
     permit changing reg classes for pseudos created by this
     simplification.  */
  lra_constraint_new_regno_start = lra_new_regno_start = max_reg_num ();
  remove_scratches ();
  scratch_p = lra_constraint_new_regno_start != max_reg_num ();

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
  push_insns (get_last_insn (), NULL_RTX);
  /* It is needed for the 1st coalescing.  */
  lra_constraint_new_insn_uid_start = get_max_uid ();
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
  for (;;)
    {
      for (;;)
	{
	  /* We should try to assign hard registers to scratches even
	     if there were no RTL transformations in
	     lra_constraints.  */
	  if (! lra_constraints (lra_constraint_iter == 0)
	      && (lra_constraint_iter > 1
		  || (! scratch_p && ! caller_save_needed)))
	    break;
	  /* Constraint transformations may result in that eliminable
	     hard regs become uneliminable and pseudos which use them
	     should be spilled.	 It is better to do it before pseudo
	     assignments.

	     For example, rs6000 can make
	     RS6000_PIC_OFFSET_TABLE_REGNUM uneliminable if we started
	     to use a constant pool.  */
	  lra_eliminate (false, false);
	  /* Do inheritance only for regular algorithms.  */
	  if (! lra_simple_p)
	    lra_inheritance ();
	  if (live_p)
	    lra_clear_live_ranges ();
	  /* We need live ranges for lra_assign -- so build them.  */
	  lra_create_live_ranges (true);
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
		      lra_create_live_ranges (true);
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
      if (! lra_need_for_spills_p ())
	break;
      if (! live_p)
	{
	  /* We need full live info for spilling pseudos into
	     registers instead of memory.  */
	  lra_create_live_ranges (lra_reg_spill_p);
	  live_p = true;
	}
      lra_spill ();
      /* Assignment of stack slots changes elimination offsets for
	 some eliminations.  So update the offsets here.  */
      lra_eliminate (false, false);
      lra_constraint_new_regno_start = max_reg_num ();
      lra_constraint_new_insn_uid_start = get_max_uid ();
      lra_constraint_iter_after_spill = 0;
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
      sbitmap blocks;
      blocks = sbitmap_alloc (last_basic_block);
      bitmap_ones (blocks);
      find_many_sub_basic_blocks (blocks);
      sbitmap_free (blocks);
    }

  if (inserted_p)
    commit_edge_insertions ();

  /* Replacing pseudos with their memory equivalents might have
     created shared rtx.  Subsequent passes would get confused
     by this, so unshare everything here.  */
  unshare_all_rtl_again (get_insns ());

#ifdef ENABLE_CHECKING
  check_rtl (true);
#endif

  timevar_pop (TV_LRA);
}

/* Called once per compiler to initialize LRA data once.  */
void
lra_init_once (void)
{
  init_insn_code_data_once ();
}

/* Initialize LRA whenever register-related information is changed.  */
void
lra_init (void)
{
  init_op_alt_data ();
}

/* Called once per compiler to finish LRA data which are initialize
   once.  */
void
lra_finish_once (void)
{
  finish_insn_code_data_once ();
}
