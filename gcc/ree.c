/* Redundant Extension Elimination pass for the GNU compiler.
   Copyright (C) 2010, 2011, 2012 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

   Based on the Redundant Zero-extension elimination pass contributed by
   Sriraman Tallam (tmsriram@google.com) and Silvius Rus (rus@google.com).

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


/* Problem Description :
   --------------------
   This pass is intended to remove redundant extension instructions.
   Such instructions appear for different reasons.  We expect some of
   them due to implicit zero-extension in 64-bit registers after writing
   to their lower 32-bit half (e.g. for the x86-64 architecture).
   Another possible reason is a type cast which follows a load (for
   instance a register restore) and which can be combined into a single
   instruction, and for which earlier local passes, e.g. the combiner,
   weren't able to optimize.

   How does this pass work  ?
   --------------------------

   This pass is run after register allocation.  Hence, all registers that
   this pass deals with are hard registers.  This pass first looks for an
   extension instruction that could possibly be redundant.  Such extension
   instructions show up in RTL with the pattern  :
   (set (reg:<SWI248> x) (any_extend:<SWI248> (reg:<SWI124> x))),
   where x can be any hard register.
   Now, this pass tries to eliminate this instruction by merging the
   extension with the definitions of register x.  For instance, if
   one of the definitions of register x was  :
   (set (reg:SI x) (plus:SI (reg:SI z1) (reg:SI z2))),
   followed by extension  :
   (set (reg:DI x) (zero_extend:DI (reg:SI x)))
   then the combination converts this into :
   (set (reg:DI x) (zero_extend:DI (plus:SI (reg:SI z1) (reg:SI z2)))).
   If all the merged definitions are recognizable assembly instructions,
   the extension is effectively eliminated.

   For example, for the x86-64 architecture, implicit zero-extensions
   are captured with appropriate patterns in the i386.md file.  Hence,
   these merged definition can be matched to a single assembly instruction.
   The original extension instruction is then deleted if all the
   definitions can be merged.

   However, there are cases where the definition instruction cannot be
   merged with an extension.  Examples are CALL instructions.  In such
   cases, the original extension is not redundant and this pass does
   not delete it.

   Handling conditional moves :
   ----------------------------

   Architectures like x86-64 support conditional moves whose semantics for
   extension differ from the other instructions.  For instance, the
   instruction *cmov ebx, eax*
   zero-extends eax onto rax only when the move from ebx to eax happens.
   Otherwise, eax may not be zero-extended.  Consider conditional moves as
   RTL instructions of the form
   (set (reg:SI x) (if_then_else (cond) (reg:SI y) (reg:SI z))).
   This pass tries to merge an extension with a conditional move by
   actually merging the definitions of y and z with an extension and then
   converting the conditional move into :
   (set (reg:DI x) (if_then_else (cond) (reg:DI y) (reg:DI z))).
   Since registers y and z are extended, register x will also be extended
   after the conditional move.  Note that this step has to be done
   transitively since the definition of a conditional copy can be
   another conditional copy.

   Motivating Example I :
   ---------------------
   For this program :
   **********************************************
   bad_code.c

   int mask[1000];

   int foo(unsigned x)
   {
     if (x < 10)
       x = x * 45;
     else
       x = x * 78;
     return mask[x];
   }
   **********************************************

   $ gcc -O2 bad_code.c
     ........
     400315:       b8 4e 00 00 00          mov    $0x4e,%eax
     40031a:       0f af f8                imul   %eax,%edi
     40031d:       89 ff                   mov    %edi,%edi - useless extension
     40031f:       8b 04 bd 60 19 40 00    mov    0x401960(,%rdi,4),%eax
     400326:       c3                      retq
     ......
     400330:       ba 2d 00 00 00          mov    $0x2d,%edx
     400335:       0f af fa                imul   %edx,%edi
     400338:       89 ff                   mov    %edi,%edi - useless extension
     40033a:       8b 04 bd 60 19 40 00    mov    0x401960(,%rdi,4),%eax
     400341:       c3                      retq

   $ gcc -O2 -free bad_code.c
     ......
     400315:       6b ff 4e                imul   $0x4e,%edi,%edi
     400318:       8b 04 bd 40 19 40 00    mov    0x401940(,%rdi,4),%eax
     40031f:       c3                      retq
     400320:       6b ff 2d                imul   $0x2d,%edi,%edi
     400323:       8b 04 bd 40 19 40 00    mov    0x401940(,%rdi,4),%eax
     40032a:       c3                      retq

   Motivating Example II :
   ---------------------

   Here is an example with a conditional move.

   For this program :
   **********************************************

   unsigned long long foo(unsigned x , unsigned y)
   {
     unsigned z;
     if (x > 100)
       z = x + y;
     else
       z = x - y;
     return (unsigned long long)(z);
   }

   $ gcc -O2 bad_code.c
     ............
     400360:       8d 14 3e                lea    (%rsi,%rdi,1),%edx
     400363:       89 f8                   mov    %edi,%eax
     400365:       29 f0                   sub    %esi,%eax
     400367:       83 ff 65                cmp    $0x65,%edi
     40036a:       0f 43 c2                cmovae %edx,%eax
     40036d:       89 c0                   mov    %eax,%eax - useless extension
     40036f:       c3                      retq

   $ gcc -O2 -free bad_code.c
     .............
     400360:       89 fa                   mov    %edi,%edx
     400362:       8d 04 3e                lea    (%rsi,%rdi,1),%eax
     400365:       29 f2                   sub    %esi,%edx
     400367:       83 ff 65                cmp    $0x65,%edi
     40036a:       89 d6                   mov    %edx,%esi
     40036c:       48 0f 42 c6             cmovb  %rsi,%rax
     400370:       c3                      retq

  Motivating Example III :
  ---------------------

  Here is an example with a type cast.

  For this program :
  **********************************************

  void test(int size, unsigned char *in, unsigned char *out)
  {
    int i;
    unsigned char xr, xg, xy=0;

    for (i = 0; i < size; i++) {
      xr = *in++;
      xg = *in++;
      xy = (unsigned char) ((19595*xr + 38470*xg) >> 16);
      *out++ = xy;
    }
  }

  $ gcc -O2 bad_code.c
    ............
    10:   0f b6 0e                movzbl (%rsi),%ecx
    13:   0f b6 46 01             movzbl 0x1(%rsi),%eax
    17:   48 83 c6 02             add    $0x2,%rsi
    1b:   0f b6 c9                movzbl %cl,%ecx - useless extension
    1e:   0f b6 c0                movzbl %al,%eax - useless extension
    21:   69 c9 8b 4c 00 00       imul   $0x4c8b,%ecx,%ecx
    27:   69 c0 46 96 00 00       imul   $0x9646,%eax,%eax

   $ gcc -O2 -free bad_code.c
     .............
    10:   0f b6 0e                movzbl (%rsi),%ecx
    13:   0f b6 46 01             movzbl 0x1(%rsi),%eax
    17:   48 83 c6 02             add    $0x2,%rsi
    1b:   69 c9 8b 4c 00 00       imul   $0x4c8b,%ecx,%ecx
    21:   69 c0 46 96 00 00       imul   $0x9646,%eax,%eax

   Usefulness :
   ----------

   The original redundant zero-extension elimination pass reported reduction
   of the dynamic instruction count of a compression benchmark by 2.8% and
   improvement of its run time by about 1%.

   The additional performance gain with the enhanced pass is mostly expected
   on in-order architectures where redundancy cannot be compensated by out of
   order execution.  Measurements showed up to 10% performance gain (reduced
   run time) on EEMBC 2.0 benchmarks on Atom processor with geomean performance
   gain 1%.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "function.h"
#include "expr.h"
#include "insn-attr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "target.h"
#include "timevar.h"
#include "optabs.h"
#include "insn-codes.h"
#include "rtlhooks-def.h"
/* Include output.h for dump_file.  */
#include "output.h"
#include "params.h"
#include "timevar.h"
#include "tree-pass.h"
#include "df.h"
#include "cgraph.h"

/* This structure represents a candidate for elimination.  */

typedef struct GTY(()) ext_cand
{
  /* The expression.  */
  const_rtx expr;

  /* The kind of extension.  */
  enum rtx_code code;

  /* The destination mode.  */
  enum machine_mode mode;

  /* The instruction where it lives.  */
  rtx insn;
} ext_cand;

DEF_VEC_O(ext_cand);
DEF_VEC_ALLOC_O(ext_cand, heap);

static int max_insn_uid;

/* Given a insn (CURR_INSN), an extension candidate for removal (CAND)
   and a pointer to the SET rtx (ORIG_SET) that needs to be modified,
   this code modifies the SET rtx to a new SET rtx that extends the
   right hand expression into a register on the left hand side.  Note
   that multiple assumptions are made about the nature of the set that
   needs to be true for this to work and is called from merge_def_and_ext.

   Original :
   (set (reg a) (expression))

   Transform :
   (set (reg a) (any_extend (expression)))

   Special Cases :
   If the expression is a constant or another extension, then directly
   assign it to the register.  */

static bool
combine_set_extension (ext_cand *cand, rtx curr_insn, rtx *orig_set)
{
  rtx orig_src = SET_SRC (*orig_set);
  rtx new_reg = gen_rtx_REG (cand->mode, REGNO (SET_DEST (*orig_set)));
  rtx new_set;

  /* Merge constants by directly moving the constant into the register under
     some conditions.  Recall that RTL constants are sign-extended.  */
  if (GET_CODE (orig_src) == CONST_INT
      && HOST_BITS_PER_WIDE_INT >= GET_MODE_BITSIZE (cand->mode))
    {
      if (INTVAL (orig_src) >= 0 || cand->code == SIGN_EXTEND)
	new_set = gen_rtx_SET (VOIDmode, new_reg, orig_src);
      else
	{
	  /* Zero-extend the negative constant by masking out the bits outside
	     the source mode.  */
	  enum machine_mode src_mode = GET_MODE (SET_DEST (*orig_set));
	  rtx new_const_int
	    = GEN_INT (INTVAL (orig_src) & GET_MODE_MASK (src_mode));
	  new_set = gen_rtx_SET (VOIDmode, new_reg, new_const_int);
	}
    }
  else if (GET_MODE (orig_src) == VOIDmode)
    {
      /* This is mostly due to a call insn that should not be optimized.  */
      return false;
    }
  else if (GET_CODE (orig_src) == cand->code)
    {
      /* Here is a sequence of two extensions.  Try to merge them.  */
      rtx temp_extension
	= gen_rtx_fmt_e (cand->code, cand->mode, XEXP (orig_src, 0));
      rtx simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (VOIDmode, new_reg, temp_extension);
    }
  else if (GET_CODE (orig_src) == IF_THEN_ELSE)
    {
      /* Only IF_THEN_ELSE of phi-type copies are combined.  Otherwise,
         in general, IF_THEN_ELSE should not be combined.  */
      return false;
    }
  else
    {
      /* This is the normal case.  */
      rtx temp_extension
	= gen_rtx_fmt_e (cand->code, cand->mode, orig_src);
      rtx simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (VOIDmode, new_reg, temp_extension);
    }

  /* This change is a part of a group of changes.  Hence,
     validate_change will not try to commit the change.  */
  if (validate_change (curr_insn, orig_set, new_set, true))
    {
      if (dump_file)
        {
          fprintf (dump_file,
		   "Tentatively merged extension with definition:\n");
          print_rtl_single (dump_file, curr_insn);
        }
      return true;
    }

  return false;
}

/* Treat if_then_else insns, where the operands of both branches
   are registers, as copies.  For instance,
   Original :
   (set (reg:SI a) (if_then_else (cond) (reg:SI b) (reg:SI c)))
   Transformed :
   (set (reg:DI a) (if_then_else (cond) (reg:DI b) (reg:DI c)))
   DEF_INSN is the if_then_else insn.  */

static bool
transform_ifelse (ext_cand *cand, rtx def_insn)
{
  rtx set_insn = PATTERN (def_insn);
  rtx srcreg, dstreg, srcreg2;
  rtx map_srcreg, map_dstreg, map_srcreg2;
  rtx ifexpr;
  rtx cond;
  rtx new_set;

  gcc_assert (GET_CODE (set_insn) == SET);

  cond = XEXP (SET_SRC (set_insn), 0);
  dstreg = SET_DEST (set_insn);
  srcreg = XEXP (SET_SRC (set_insn), 1);
  srcreg2 = XEXP (SET_SRC (set_insn), 2);
  /* If the conditional move already has the right or wider mode,
     there is nothing to do.  */
  if (GET_MODE_SIZE (GET_MODE (dstreg)) >= GET_MODE_SIZE (cand->mode))
    return true;

  map_srcreg = gen_rtx_REG (cand->mode, REGNO (srcreg));
  map_srcreg2 = gen_rtx_REG (cand->mode, REGNO (srcreg2));
  map_dstreg = gen_rtx_REG (cand->mode, REGNO (dstreg));
  ifexpr = gen_rtx_IF_THEN_ELSE (cand->mode, cond, map_srcreg, map_srcreg2);
  new_set = gen_rtx_SET (VOIDmode, map_dstreg, ifexpr);

  if (validate_change (def_insn, &PATTERN (def_insn), new_set, true))
    {
      if (dump_file)
        {
          fprintf (dump_file,
		   "Mode of conditional move instruction extended:\n");
          print_rtl_single (dump_file, def_insn);
        }
      return true;
    }

  return false;
}

/* Get all the reaching definitions of an instruction.  The definitions are
   desired for REG used in INSN.  Return the definition list or NULL if a
   definition is missing.  If DEST is non-NULL, additionally push the INSN
   of the definitions onto DEST.  */

static struct df_link *
get_defs (rtx insn, rtx reg, VEC (rtx,heap) **dest)
{
  df_ref reg_info, *uses;
  struct df_link *ref_chain, *ref_link;

  reg_info = NULL;

  for (uses = DF_INSN_USES (insn); *uses; uses++)
    {
      reg_info = *uses;
      if (GET_CODE (DF_REF_REG (reg_info)) == SUBREG)
        return NULL;
      if (REGNO (DF_REF_REG (reg_info)) == REGNO (reg))
        break;
    }

  gcc_assert (reg_info != NULL && uses != NULL);

  ref_chain = DF_REF_CHAIN (reg_info);

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some definition for this instruction.  */
      if (ref_link->ref == NULL)
        return NULL;
      if (DF_REF_INSN_INFO (ref_link->ref) == NULL)
        return NULL;
    }

  if (dest)
    for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
      VEC_safe_push (rtx, heap, *dest, DF_REF_INSN (ref_link->ref));

  return ref_chain;
}

/* Return true if INSN is
     (SET (reg REGNO (def_reg)) (if_then_else (cond) (REG x1) (REG x2)))
   and store x1 and x2 in REG_1 and REG_2.  */

static bool
is_cond_copy_insn (rtx insn, rtx *reg1, rtx *reg2)
{
  rtx expr = single_set (insn);

  if (expr != NULL_RTX
      && GET_CODE (expr) == SET
      && GET_CODE (SET_DEST (expr)) == REG
      && GET_CODE (SET_SRC (expr))  == IF_THEN_ELSE
      && GET_CODE (XEXP (SET_SRC (expr), 1)) == REG
      && GET_CODE (XEXP (SET_SRC (expr), 2)) == REG)
    {
      *reg1 = XEXP (SET_SRC (expr), 1);
      *reg2 = XEXP (SET_SRC (expr), 2);
      return true;
    }

  return false;
}

enum ext_modified_kind
{
  /* The insn hasn't been modified by ree pass yet.  */
  EXT_MODIFIED_NONE,
  /* Changed into zero extension.  */
  EXT_MODIFIED_ZEXT,
  /* Changed into sign extension.  */
  EXT_MODIFIED_SEXT
};

struct ext_modified
{
  /* Mode from which ree has zero or sign extended the destination.  */
  ENUM_BITFIELD(machine_mode) mode : 8;

  /* Kind of modification of the insn.  */
  ENUM_BITFIELD(ext_modified_kind) kind : 2;

  /* True if the insn is scheduled to be deleted.  */
  unsigned int deleted : 1;
};

/* Vectors used by combine_reaching_defs and its helpers.  */
typedef struct ext_state
{
  /* In order to avoid constant VEC_alloc/VEC_free, we keep these
     4 vectors live through the entire find_and_remove_re and just
     VEC_truncate them each time.  */
  VEC (rtx, heap) *defs_list;
  VEC (rtx, heap) *copies_list;
  VEC (rtx, heap) *modified_list;
  VEC (rtx, heap) *work_list;

  /* For instructions that have been successfully modified, this is
     the original mode from which the insn is extending and
     kind of extension.  */
  struct ext_modified *modified;
} ext_state;

/* Reaching Definitions of the extended register could be conditional copies
   or regular definitions.  This function separates the two types into two
   lists, STATE->DEFS_LIST and STATE->COPIES_LIST.  This is necessary because,
   if a reaching definition is a conditional copy, merging the extension with
   this definition is wrong.  Conditional copies are merged by transitively
   merging their definitions.  The defs_list is populated with all the reaching
   definitions of the extension instruction (EXTEND_INSN) which must be merged
   with an extension.  The copies_list contains all the conditional moves that
   will later be extended into a wider mode conditional move if all the merges
   are successful.  The function returns false upon failure, true upon
   success.  */

static bool
make_defs_and_copies_lists (rtx extend_insn, const_rtx set_pat,
			    ext_state *state)
{
  rtx src_reg = XEXP (SET_SRC (set_pat), 0);
  bool *is_insn_visited;
  bool ret = true;

  VEC_truncate (rtx, state->work_list, 0);

  /* Initialize the work list.  */
  if (!get_defs (extend_insn, src_reg, &state->work_list))
    gcc_unreachable ();

  is_insn_visited = XCNEWVEC (bool, max_insn_uid);

  /* Perform transitive closure for conditional copies.  */
  while (!VEC_empty (rtx, state->work_list))
    {
      rtx def_insn = VEC_pop (rtx, state->work_list);
      rtx reg1, reg2;

      gcc_assert (INSN_UID (def_insn) < max_insn_uid);

      if (is_insn_visited[INSN_UID (def_insn)])
	continue;
      is_insn_visited[INSN_UID (def_insn)] = true;

      if (is_cond_copy_insn (def_insn, &reg1, &reg2))
	{
	  /* Push it onto the copy list first.  */
	  VEC_safe_push (rtx, heap, state->copies_list, def_insn);

	  /* Now perform the transitive closure.  */
	  if (!get_defs (def_insn, reg1, &state->work_list)
	      || !get_defs (def_insn, reg2, &state->work_list))
	    {
	      ret = false;
	      break;
	    }
        }
      else
	VEC_safe_push (rtx, heap, state->defs_list, def_insn);
    }

  XDELETEVEC (is_insn_visited);

  return ret;
}

/* Merge the DEF_INSN with an extension.  Calls combine_set_extension
   on the SET pattern.  */

static bool
merge_def_and_ext (ext_cand *cand, rtx def_insn, ext_state *state)
{
  enum machine_mode ext_src_mode;
  enum rtx_code code;
  rtx *sub_rtx;
  rtx s_expr;
  int i;

  ext_src_mode = GET_MODE (XEXP (SET_SRC (cand->expr), 0));
  code = GET_CODE (PATTERN (def_insn));
  sub_rtx = NULL;

  if (code == PARALLEL)
    {
      for (i = 0; i < XVECLEN (PATTERN (def_insn), 0); i++)
        {
          s_expr = XVECEXP (PATTERN (def_insn), 0, i);
          if (GET_CODE (s_expr) != SET)
            continue;

          if (sub_rtx == NULL)
            sub_rtx = &XVECEXP (PATTERN (def_insn), 0, i);
          else
            {
              /* PARALLEL with multiple SETs.  */
              return false;
            }
        }
    }
  else if (code == SET)
    sub_rtx = &PATTERN (def_insn);
  else
    {
      /* It is not a PARALLEL or a SET, what could it be ? */
      return false;
    }

  gcc_assert (sub_rtx != NULL);

  if (REG_P (SET_DEST (*sub_rtx))
      && (GET_MODE (SET_DEST (*sub_rtx)) == ext_src_mode
	  || ((state->modified[INSN_UID (def_insn)].kind
	       == (cand->code == ZERO_EXTEND
		   ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT))
	      && state->modified[INSN_UID (def_insn)].mode
		 == ext_src_mode)))
    {
      if (GET_MODE_SIZE (GET_MODE (SET_DEST (*sub_rtx)))
	  >= GET_MODE_SIZE (cand->mode))
	return true;
      /* If def_insn is already scheduled to be deleted, don't attempt
	 to modify it.  */
      if (state->modified[INSN_UID (def_insn)].deleted)
	return false;
      if (combine_set_extension (cand, def_insn, sub_rtx))
	{
	  if (state->modified[INSN_UID (def_insn)].kind == EXT_MODIFIED_NONE)
	    state->modified[INSN_UID (def_insn)].mode = ext_src_mode;
	  return true;
	}
    }

  return false;
}

/* This function goes through all reaching defs of the source
   of the candidate for elimination (CAND) and tries to combine
   the extension with the definition instruction.  The changes
   are made as a group so that even if one definition cannot be
   merged, all reaching definitions end up not being merged.
   When a conditional copy is encountered, merging is attempted
   transitively on its definitions.  It returns true upon success
   and false upon failure.  */

static bool
combine_reaching_defs (ext_cand *cand, const_rtx set_pat, ext_state *state)
{
  rtx def_insn;
  bool merge_successful = true;
  int i;
  int defs_ix;
  bool outcome;

  VEC_truncate (rtx, state->defs_list, 0);
  VEC_truncate (rtx, state->copies_list, 0);

  outcome = make_defs_and_copies_lists (cand->insn, set_pat, state);

  if (!outcome)
    return false;

  /* If cand->insn has been already modified, update cand->mode to a wider
     mode if possible, or punt.  */
  if (state->modified[INSN_UID (cand->insn)].kind != EXT_MODIFIED_NONE)
    {
      enum machine_mode mode;
      rtx set;

      if (state->modified[INSN_UID (cand->insn)].kind
	  != (cand->code == ZERO_EXTEND
	      ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT)
	  || state->modified[INSN_UID (cand->insn)].mode != cand->mode
	  || (set = single_set (cand->insn)) == NULL_RTX)
	return false;
      mode = GET_MODE (SET_DEST (set));
      gcc_assert (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (cand->mode));
      cand->mode = mode;
    }

  merge_successful = true;

  /* Go through the defs vector and try to merge all the definitions
     in this vector.  */
  VEC_truncate (rtx, state->modified_list, 0);
  FOR_EACH_VEC_ELT (rtx, state->defs_list, defs_ix, def_insn)
    {
      if (merge_def_and_ext (cand, def_insn, state))
	VEC_safe_push (rtx, heap, state->modified_list, def_insn);
      else
        {
          merge_successful = false;
          break;
        }
    }

  /* Now go through the conditional copies vector and try to merge all
     the copies in this vector.  */
  if (merge_successful)
    {
      FOR_EACH_VEC_ELT (rtx, state->copies_list, i, def_insn)
        {
          if (transform_ifelse (cand, def_insn))
	    VEC_safe_push (rtx, heap, state->modified_list, def_insn);
          else
            {
              merge_successful = false;
              break;
            }
        }
    }

  if (merge_successful)
    {
      /* Commit the changes here if possible
	 FIXME: It's an all-or-nothing scenario.  Even if only one definition
	 cannot be merged, we entirely give up.  In the future, we should allow
	 extensions to be partially eliminated along those paths where the
	 definitions could be merged.  */
      if (apply_change_group ())
        {
          if (dump_file)
            fprintf (dump_file, "All merges were successful.\n");

	  FOR_EACH_VEC_ELT (rtx, state->modified_list, i, def_insn)
	    if (state->modified[INSN_UID (def_insn)].kind == EXT_MODIFIED_NONE)
	      state->modified[INSN_UID (def_insn)].kind
		= (cand->code == ZERO_EXTEND
		   ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT);

          return true;
        }
      else
        {
          /* Changes need not be cancelled explicitly as apply_change_group
             does it.  Print list of definitions in the dump_file for debug
             purposes.  This extension cannot be deleted.  */
          if (dump_file)
            {
	      fprintf (dump_file,
		       "Merge cancelled, non-mergeable definitions:\n");
	      FOR_EACH_VEC_ELT (rtx, state->modified_list, i, def_insn)
	        print_rtl_single (dump_file, def_insn);
            }
        }
    }
  else
    {
      /* Cancel any changes that have been made so far.  */
      cancel_changes (0);
    }

  return false;
}

/* Add an extension pattern that could be eliminated.  */

static void
add_removable_extension (const_rtx expr, rtx insn,
			 VEC (ext_cand, heap) **insn_list,
			 unsigned *def_map)
{
  enum rtx_code code;
  enum machine_mode mode;
  unsigned int idx;
  rtx src, dest;

  /* We are looking for SET (REG N) (ANY_EXTEND (REG N)).  */
  if (GET_CODE (expr) != SET)
    return;

  src = SET_SRC (expr);
  code = GET_CODE (src);
  dest = SET_DEST (expr);
  mode = GET_MODE (dest);

  if (REG_P (dest)
      && (code == SIGN_EXTEND || code == ZERO_EXTEND)
      && REG_P (XEXP (src, 0))
      && REGNO (dest) == REGNO (XEXP (src, 0)))
    {
      struct df_link *defs, *def;
      ext_cand *cand;

      /* First, make sure we can get all the reaching definitions.  */
      defs = get_defs (insn, XEXP (src, 0), NULL);
      if (!defs)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Cannot eliminate extension:\n");
	      print_rtl_single (dump_file, insn);
	      fprintf (dump_file, " because of missing definition(s)\n");
	    }
	  return;
	}

      /* Second, make sure the reaching definitions don't feed another and
	 different extension.  FIXME: this obviously can be improved.  */
      for (def = defs; def; def = def->next)
	if ((idx = def_map[INSN_UID(DF_REF_INSN (def->ref))])
	    && (cand = VEC_index (ext_cand, *insn_list, idx - 1))
	    && (cand->code != code || cand->mode != mode))
	  {
	    if (dump_file)
	      {
	        fprintf (dump_file, "Cannot eliminate extension:\n");
		print_rtl_single (dump_file, insn);
	        fprintf (dump_file, " because of other extension\n");
	      }
	    return;
	  }

      /* Then add the candidate to the list and insert the reaching definitions
         into the definition map.  */
      cand = VEC_safe_push (ext_cand, heap, *insn_list, NULL);
      cand->expr = expr;
      cand->code = code;
      cand->mode = mode;
      cand->insn = insn;
      idx = VEC_length (ext_cand, *insn_list);

      for (def = defs; def; def = def->next)
	def_map[INSN_UID(DF_REF_INSN (def->ref))] = idx;
    }
}

/* Traverse the instruction stream looking for extensions and return the
   list of candidates.  */

static VEC (ext_cand, heap)*
find_removable_extensions (void)
{
  VEC (ext_cand, heap) *insn_list = NULL;
  basic_block bb;
  rtx insn, set;
  unsigned *def_map = XCNEWVEC (unsigned, max_insn_uid);

  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
      {
	if (!NONDEBUG_INSN_P (insn))
	  continue;

	set = single_set (insn);
	if (set == NULL_RTX)
	  continue;
	add_removable_extension (set, insn, &insn_list, def_map);
      }

  XDELETEVEC (def_map);

  return insn_list;
}

/* This is the main function that checks the insn stream for redundant
   extensions and tries to remove them if possible.  */

static void
find_and_remove_re (void)
{
  ext_cand *curr_cand;
  rtx curr_insn = NULL_RTX;
  int num_re_opportunities = 0, num_realized = 0, i;
  VEC (ext_cand, heap) *reinsn_list;
  VEC (rtx, heap) *reinsn_del_list;
  ext_state state;

  /* Construct DU chain to get all reaching definitions of each
     extension instruction.  */
  df_chain_add_problem (DF_UD_CHAIN + DF_DU_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  max_insn_uid = get_max_uid ();
  reinsn_del_list = NULL;
  reinsn_list = find_removable_extensions ();
  state.defs_list = NULL;
  state.copies_list = NULL;
  state.modified_list = NULL;
  state.work_list = NULL;
  if (VEC_empty (ext_cand, reinsn_list))
    state.modified = NULL;
  else
    state.modified = XCNEWVEC (struct ext_modified, max_insn_uid);

  FOR_EACH_VEC_ELT (ext_cand, reinsn_list, i, curr_cand)
    {
      num_re_opportunities++;

      /* Try to combine the extension with the definition.  */
      if (dump_file)
        {
          fprintf (dump_file, "Trying to eliminate extension:\n");
          print_rtl_single (dump_file, curr_cand->insn);
        }

      if (combine_reaching_defs (curr_cand, curr_cand->expr, &state))
        {
          if (dump_file)
            fprintf (dump_file, "Eliminated the extension.\n");
          num_realized++;
          VEC_safe_push (rtx, heap, reinsn_del_list, curr_cand->insn);
	  state.modified[INSN_UID (curr_cand->insn)].deleted = 1;
        }
    }

  /* Delete all useless extensions here in one sweep.  */
  FOR_EACH_VEC_ELT (rtx, reinsn_del_list, i, curr_insn)
    delete_insn (curr_insn);

  VEC_free (ext_cand, heap, reinsn_list);
  VEC_free (rtx, heap, reinsn_del_list);
  VEC_free (rtx, heap, state.defs_list);
  VEC_free (rtx, heap, state.copies_list);
  VEC_free (rtx, heap, state.modified_list);
  VEC_free (rtx, heap, state.work_list);
  XDELETEVEC (state.modified);

  if (dump_file && num_re_opportunities > 0)
    fprintf (dump_file, "Elimination opportunities = %d realized = %d\n",
	     num_re_opportunities, num_realized);

  df_finish_pass (false);
}

/* Find and remove redundant extensions.  */

static unsigned int
rest_of_handle_ree (void)
{
  timevar_push (TV_REE);
  find_and_remove_re ();
  timevar_pop (TV_REE);
  return 0;
}

/* Run REE pass when flag_ree is set at optimization level > 0.  */

static bool
gate_handle_ree (void)
{
  return (optimize > 0 && flag_ree);
}

struct rtl_opt_pass pass_ree =
{
 {
  RTL_PASS,
  "ree",                                /* name */
  gate_handle_ree,                      /* gate */
  rest_of_handle_ree,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_REE,                               /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect |
  TODO_verify_rtl_sharing,              /* todo_flags_finish */
 }
};
