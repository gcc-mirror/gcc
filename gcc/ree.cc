/* Redundant Extension Elimination pass for the GNU compiler.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "expr.h"
#include "tree-pass.h"

/* This structure represents a candidate for elimination.  */

struct ext_cand
{
  /* The expression.  */
  const_rtx expr;

  /* The kind of extension.  */
  enum rtx_code code;

  /* The destination mode.  */
  machine_mode mode;

  /* The instruction where it lives.  */
  rtx_insn *insn;
};


static int max_insn_uid;

/* Update or remove REG_EQUAL or REG_EQUIV notes for INSN.  */

static bool
update_reg_equal_equiv_notes (rtx_insn *insn, machine_mode new_mode,
			      machine_mode old_mode, enum rtx_code code)
{
  rtx *loc = &REG_NOTES (insn);
  while (*loc)
    {
      enum reg_note kind = REG_NOTE_KIND (*loc);
      if (kind == REG_EQUAL || kind == REG_EQUIV)
	{
	  rtx orig_src = XEXP (*loc, 0);
	  /* Update equivalency constants.  Recall that RTL constants are
	     sign-extended.  */
	  if (GET_CODE (orig_src) == CONST_INT
	      && HWI_COMPUTABLE_MODE_P (new_mode))
	    {
	      if (INTVAL (orig_src) >= 0 || code == SIGN_EXTEND)
		/* Nothing needed.  */;
	      else
		{
		  /* Zero-extend the negative constant by masking out the
		     bits outside the source mode.  */
		  rtx new_const_int
		    = gen_int_mode (INTVAL (orig_src)
				    & GET_MODE_MASK (old_mode),
				    new_mode);
		  if (!validate_change (insn, &XEXP (*loc, 0),
					new_const_int, true))
		    return false;
		}
	      loc = &XEXP (*loc, 1);
	    }
	  /* Drop all other notes, they assume a wrong mode.  */
	  else if (!validate_change (insn, loc, XEXP (*loc, 1), true))
	    return false;
	}
      else
	loc = &XEXP (*loc, 1);
    }
  return true;
}

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
combine_set_extension (ext_cand *cand, rtx_insn *curr_insn, rtx *orig_set)
{
  rtx orig_src = SET_SRC (*orig_set);
  machine_mode orig_mode = GET_MODE (SET_DEST (*orig_set));
  rtx new_set;
  rtx cand_pat = single_set (cand->insn);

  /* If the extension's source/destination registers are not the same
     then we need to change the original load to reference the destination
     of the extension.  Then we need to emit a copy from that destination
     to the original destination of the load.  */
  rtx new_reg;
  bool copy_needed
    = (REGNO (SET_DEST (cand_pat)) != REGNO (XEXP (SET_SRC (cand_pat), 0)));
  if (copy_needed)
    new_reg = gen_rtx_REG (cand->mode, REGNO (SET_DEST (cand_pat)));
  else
    new_reg = gen_rtx_REG (cand->mode, REGNO (SET_DEST (*orig_set)));

  /* Merge constants by directly moving the constant into the register under
     some conditions.  Recall that RTL constants are sign-extended.  */
  if (GET_CODE (orig_src) == CONST_INT
      && HWI_COMPUTABLE_MODE_P (cand->mode))
    {
      if (INTVAL (orig_src) >= 0 || cand->code == SIGN_EXTEND)
	new_set = gen_rtx_SET (new_reg, orig_src);
      else
	{
	  /* Zero-extend the negative constant by masking out the bits outside
	     the source mode.  */
	  rtx new_const_int
	    = gen_int_mode (INTVAL (orig_src) & GET_MODE_MASK (orig_mode),
			    GET_MODE (new_reg));
	  new_set = gen_rtx_SET (new_reg, new_const_int);
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
      new_set = gen_rtx_SET (new_reg, temp_extension);
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
      new_set = gen_rtx_SET (new_reg, temp_extension);
    }

  /* This change is a part of a group of changes.  Hence,
     validate_change will not try to commit the change.  */
  if (validate_change (curr_insn, orig_set, new_set, true)
      && update_reg_equal_equiv_notes (curr_insn, cand->mode, orig_mode,
				       cand->code))
    {
      if (dump_file)
        {
          fprintf (dump_file,
		   "Tentatively merged extension with definition %s:\n",
		   (copy_needed) ? "(copy needed)" : "");
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
transform_ifelse (ext_cand *cand, rtx_insn *def_insn)
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
  if (GET_MODE_UNIT_SIZE (GET_MODE (dstreg))
      >= GET_MODE_UNIT_SIZE (cand->mode))
    return true;

  map_srcreg = gen_rtx_REG (cand->mode, REGNO (srcreg));
  map_srcreg2 = gen_rtx_REG (cand->mode, REGNO (srcreg2));
  map_dstreg = gen_rtx_REG (cand->mode, REGNO (dstreg));
  ifexpr = gen_rtx_IF_THEN_ELSE (cand->mode, cond, map_srcreg, map_srcreg2);
  new_set = gen_rtx_SET (map_dstreg, ifexpr);

  if (validate_change (def_insn, &PATTERN (def_insn), new_set, true)
      && update_reg_equal_equiv_notes (def_insn, cand->mode, GET_MODE (dstreg),
				       cand->code))
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
get_defs (rtx_insn *insn, rtx reg, vec<rtx_insn *> *dest)
{
  df_ref use;
  struct df_link *ref_chain, *ref_link;

  FOR_EACH_INSN_USE (use, insn)
    {
      if (GET_CODE (DF_REF_REG (use)) == SUBREG)
        return NULL;
      if (REGNO (DF_REF_REG (use)) == REGNO (reg))
	break;
    }

  gcc_assert (use != NULL);

  ref_chain = DF_REF_CHAIN (use);

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some definition for this instruction.  */
      if (ref_link->ref == NULL)
        return NULL;
      if (DF_REF_INSN_INFO (ref_link->ref) == NULL)
        return NULL;
      /* As global regs are assumed to be defined at each function call
	 dataflow can report a call_insn as being a definition of REG.
	 But we can't do anything with that in this pass so proceed only
	 if the instruction really sets REG in a way that can be deduced
	 from the RTL structure.  */
      if (global_regs[REGNO (reg)]
	  && !set_of (reg, DF_REF_INSN (ref_link->ref)))
	return NULL;
    }

  if (dest)
    for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
      dest->safe_push (DF_REF_INSN (ref_link->ref));

  return ref_chain;
}

/* Get all the reaching uses of an instruction.  The uses are desired for REG
   set in INSN.  Return use list or NULL if a use is missing or irregular.  */

static struct df_link *
get_uses (rtx_insn *insn, rtx reg)
{
  df_ref def;
  struct df_link *ref_chain, *ref_link;

  FOR_EACH_INSN_DEF (def, insn)
    if (REGNO (DF_REF_REG (def)) == REGNO (reg))
      break;

  gcc_assert (def != NULL);

  ref_chain = DF_REF_CHAIN (def);

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some use for this instruction.  */
      if (ref_link->ref == NULL)
        return NULL;
      if (DF_REF_CLASS (ref_link->ref) != DF_REF_REGULAR)
	return NULL;
    }

  return ref_chain;
}

/* Return true if INSN is
     (SET (reg REGNO (def_reg)) (if_then_else (cond) (REG x1) (REG x2)))
   and store x1 and x2 in REG_1 and REG_2.  */

static bool
is_cond_copy_insn (rtx_insn *insn, rtx *reg1, rtx *reg2)
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
  ENUM_BITFIELD(machine_mode) mode : MACHINE_MODE_BITSIZE;

  /* Kind of modification of the insn.  */
  ENUM_BITFIELD(ext_modified_kind) kind : 2;

  unsigned int do_not_reextend : 1;

  /* True if the insn is scheduled to be deleted.  */
  unsigned int deleted : 1;
};

/* Vectors used by combine_reaching_defs and its helpers.  */
class ext_state
{
public:
  /* In order to avoid constant alloc/free, we keep these
     4 vectors live through the entire find_and_remove_re and just
     truncate them each time.  */
  auto_vec<rtx_insn *> defs_list;
  auto_vec<rtx_insn *> copies_list;
  auto_vec<rtx_insn *> modified_list;
  auto_vec<rtx_insn *> work_list;

  /* For instructions that have been successfully modified, this is
     the original mode from which the insn is extending and
     kind of extension.  */
  struct ext_modified *modified;
};

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
make_defs_and_copies_lists (rtx_insn *extend_insn, const_rtx set_pat,
			    ext_state *state)
{
  rtx src_reg = XEXP (SET_SRC (set_pat), 0);
  bool *is_insn_visited;
  bool ret = true;

  state->work_list.truncate (0);

  /* Initialize the work list.  */
  if (!get_defs (extend_insn, src_reg, &state->work_list))
    return false;

  is_insn_visited = XCNEWVEC (bool, max_insn_uid);

  /* Perform transitive closure for conditional copies.  */
  while (!state->work_list.is_empty ())
    {
      rtx_insn *def_insn = state->work_list.pop ();
      rtx reg1, reg2;

      gcc_assert (INSN_UID (def_insn) < max_insn_uid);

      if (is_insn_visited[INSN_UID (def_insn)])
	continue;
      is_insn_visited[INSN_UID (def_insn)] = true;

      if (is_cond_copy_insn (def_insn, &reg1, &reg2))
	{
	  /* Push it onto the copy list first.  */
	  state->copies_list.safe_push (def_insn);

	  /* Now perform the transitive closure.  */
	  if (!get_defs (def_insn, reg1, &state->work_list)
	      || !get_defs (def_insn, reg2, &state->work_list))
	    {
	      ret = false;
	      break;
	    }
        }
      else
	state->defs_list.safe_push (def_insn);
    }

  XDELETEVEC (is_insn_visited);

  return ret;
}

/* If DEF_INSN has single SET expression with a register
   destination, possibly buried inside a PARALLEL, return
   the address of the SET expression, else return NULL.
   This is similar to single_set, except that single_set
   allows multiple SETs when all but one is dead.  */
static rtx *
get_sub_rtx (rtx_insn *def_insn)
{
  enum rtx_code code = GET_CODE (PATTERN (def_insn));
  rtx *sub_rtx = NULL;

  if (code == PARALLEL)
    {
      for (int i = 0; i < XVECLEN (PATTERN (def_insn), 0); i++)
        {
          rtx s_expr = XVECEXP (PATTERN (def_insn), 0, i);
          if (GET_CODE (s_expr) != SET)
            continue;
	  if (!REG_P (SET_DEST (s_expr)))
	    continue;

          if (sub_rtx == NULL)
            sub_rtx = &XVECEXP (PATTERN (def_insn), 0, i);
          else
            {
              /* PARALLEL with multiple SETs.  */
              return NULL;
            }
        }
    }
  else if (code == SET)
    {
	rtx s_expr = PATTERN (def_insn);
	if (REG_P (SET_DEST (s_expr)))
	  sub_rtx = &PATTERN (def_insn);
    }

  return sub_rtx;
}

/* Merge the DEF_INSN with an extension.  Calls combine_set_extension
   on the SET pattern.  */

static bool
merge_def_and_ext (ext_cand *cand, rtx_insn *def_insn, ext_state *state)
{
  machine_mode ext_src_mode;
  rtx *sub_rtx;

  ext_src_mode = GET_MODE (XEXP (SET_SRC (cand->expr), 0));
  sub_rtx = get_sub_rtx (def_insn);

  if (sub_rtx == NULL)
    return false;

  if (GET_MODE (SET_DEST (*sub_rtx)) == ext_src_mode
	  || ((state->modified[INSN_UID (def_insn)].kind
	       == (cand->code == ZERO_EXTEND
		   ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT))
	      && state->modified[INSN_UID (def_insn)].mode
		 == ext_src_mode))
    {
      if (GET_MODE_UNIT_SIZE (GET_MODE (SET_DEST (*sub_rtx)))
	  >= GET_MODE_UNIT_SIZE (cand->mode))
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

/* Given SRC, which should be one or more extensions of a REG, strip
   away the extensions and return the REG.  */

static inline rtx
get_extended_src_reg (rtx src)
{
  while (GET_CODE (src) == SIGN_EXTEND || GET_CODE (src) == ZERO_EXTEND)
    src = XEXP (src, 0);
  gcc_assert (REG_P (src));
  return src;
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
  rtx_insn *def_insn;
  bool merge_successful = true;
  int i;
  int defs_ix;
  bool outcome;

  state->defs_list.truncate (0);
  state->copies_list.truncate (0);

  outcome = make_defs_and_copies_lists (cand->insn, set_pat, state);

  if (!outcome)
    return false;

  /* If the destination operand of the extension is a different
     register than the source operand, then additional restrictions
     are needed.  Note we have to handle cases where we have nested
     extensions in the source operand.

     Candidate insns are known to be single_sets, via the test in
     find_removable_extensions.  So we continue to use single_set here
     rather than get_sub_rtx.  */
  rtx set = single_set (cand->insn);
  bool copy_needed
    = (REGNO (SET_DEST (set)) != REGNO (get_extended_src_reg (SET_SRC (set))));
  if (copy_needed)
    {
      /* Considering transformation of
	 (set (reg1) (expression))
	 ...
	 (set (reg2) (any_extend (reg1)))

	 into

	 (set (reg2) (any_extend (expression)))
	 (set (reg1) (reg2))
	 ...  */

      /* In theory we could handle more than one reaching def, it
	 just makes the code to update the insn stream more complex.  */
      if (state->defs_list.length () != 1)
	return false;

      /* We don't have the structure described above if there are
	 conditional moves in between the def and the candidate,
	 and we will not handle them correctly.  See PR68194.  */
      if (state->copies_list.length () > 0)
	return false;

      /* We require the candidate not already be modified.  It may,
	 for example have been changed from a (sign_extend (reg))
	 into (zero_extend (sign_extend (reg))).

	 Handling that case shouldn't be terribly difficult, but the code
	 here and the code to emit copies would need auditing.  Until
	 we see a need, this is the safe thing to do.  */
      if (state->modified[INSN_UID (cand->insn)].kind != EXT_MODIFIED_NONE)
	return false;

      machine_mode dst_mode = GET_MODE (SET_DEST (set));
      rtx src_reg = get_extended_src_reg (SET_SRC (set));

      /* Ensure we can use the src_reg in dst_mode (needed for
	 the (set (reg1) (reg2)) insn mentioned above).  */
      if (!targetm.hard_regno_mode_ok (REGNO (src_reg), dst_mode))
	return false;

      /* Ensure the number of hard registers of the copy match.  */
      if (hard_regno_nregs (REGNO (src_reg), dst_mode) != REG_NREGS (src_reg))
	return false;

      /* There's only one reaching def.  */
      rtx_insn *def_insn = state->defs_list[0];

      /* The defining statement must not have been modified either.  */
      if (state->modified[INSN_UID (def_insn)].kind != EXT_MODIFIED_NONE)
	return false;

      /* The defining statement and candidate insn must be in the same block.
	 This is merely to keep the test for safety and updating the insn
	 stream simple.  Also ensure that within the block the candidate
	 follows the defining insn.  */
      basic_block bb = BLOCK_FOR_INSN (cand->insn);
      if (bb != BLOCK_FOR_INSN (def_insn)
	  || DF_INSN_LUID (def_insn) > DF_INSN_LUID (cand->insn))
	return false;

      /* If there is an overlap between the destination of DEF_INSN and
	 CAND->insn, then this transformation is not safe.  Note we have
	 to test in the widened mode.  */
      rtx *dest_sub_rtx = get_sub_rtx (def_insn);
      if (dest_sub_rtx == NULL)
	return false;

      rtx tmp_reg = gen_rtx_REG (GET_MODE (SET_DEST (set)),
				 REGNO (SET_DEST (*dest_sub_rtx)));
      if (reg_overlap_mentioned_p (tmp_reg, SET_DEST (set)))
	return false;

      /* On RISC machines we must make sure that changing the mode of SRC_REG
	 as destination register will not affect its reaching uses, which may
	 read its value in a larger mode because DEF_INSN implicitly sets it
	 in word mode.  */
      poly_int64 prec
	= GET_MODE_PRECISION (GET_MODE (SET_DEST (*dest_sub_rtx)));
      if (WORD_REGISTER_OPERATIONS && known_lt (prec, BITS_PER_WORD))
	{
	  struct df_link *uses = get_uses (def_insn, src_reg);
	  if (!uses)
	    return false;

	  for (df_link *use = uses; use; use = use->next)
	    if (paradoxical_subreg_p (GET_MODE (*DF_REF_LOC (use->ref)),
				      GET_MODE (SET_DEST (*dest_sub_rtx)))
		&& !DEBUG_INSN_P (DF_REF_INSN (use->ref)))
	      return false;
	}

      /* The destination register of the extension insn must not be
	 used or set between the def_insn and cand->insn exclusive.  */
      if (reg_used_between_p (SET_DEST (set), def_insn, cand->insn)
	  || reg_set_between_p (SET_DEST (set), def_insn, cand->insn))
	return false;

      /* We must be able to copy between the two registers.   Generate,
	 recognize and verify constraints of the copy.  Also fail if this
	 generated more than one insn.

         This generates garbage since we throw away the insn when we're
	 done, only to recreate it later if this test was successful.

	 Make sure to get the mode from the extension (cand->insn).  This
	 is different than in the code to emit the copy as we have not
	 modified the defining insn yet.  */
      start_sequence ();
      rtx new_dst = gen_rtx_REG (GET_MODE (SET_DEST (set)),
                                 REGNO (get_extended_src_reg (SET_SRC (set))));
      rtx new_src = gen_rtx_REG (GET_MODE (SET_DEST (set)),
                                 REGNO (SET_DEST (set)));
      emit_move_insn (new_dst, new_src);

      rtx_insn *insn = get_insns ();
      end_sequence ();
      if (NEXT_INSN (insn))
	return false;
      if (recog_memoized (insn) == -1)
	return false;
      extract_insn (insn);
      if (!constrain_operands (1, get_preferred_alternatives (insn, bb)))
	return false;

      while (REG_P (SET_SRC (*dest_sub_rtx))
	     && (REGNO (SET_SRC (*dest_sub_rtx)) == REGNO (SET_DEST (set))))
	{
	  /* Considering transformation of
	     (set (reg2) (expression))
	     ...
	     (set (reg1) (reg2))
	     ...
	     (set (reg2) (any_extend (reg1)))

	     into

	     (set (reg2) (any_extend (expression)))
	     (set (reg1) (reg2))
	     ...  */
	  struct df_link *defs
	    = get_defs (def_insn, SET_SRC (*dest_sub_rtx), NULL);
	  if (defs == NULL || defs->next)
	    break;

	  /* There is only one reaching def.  */
	  rtx_insn *def_insn2 = DF_REF_INSN (defs->ref);

	  /* The defining statement must not have been modified either.  */
	  if (state->modified[INSN_UID (def_insn2)].kind != EXT_MODIFIED_NONE)
	    break;

	  /* The def_insn2 and candidate insn must be in the same
	     block and def_insn follows def_insn2.  */
	  if (bb != BLOCK_FOR_INSN (def_insn2)
	      || DF_INSN_LUID (def_insn2) > DF_INSN_LUID (def_insn))
	    break;

	  rtx *dest_sub_rtx2 = get_sub_rtx (def_insn2);
	  if (dest_sub_rtx2 == NULL)
	    break;

	  /* On RISC machines we must make sure that changing the mode of
	     SRC_REG as destination register will not affect its reaching
	     uses, which may read its value in a larger mode because DEF_INSN
	     implicitly sets it in word mode.  */
	  if (WORD_REGISTER_OPERATIONS && known_lt (prec, BITS_PER_WORD))
	    {
	      struct df_link *uses = get_uses (def_insn2, SET_DEST (set));
	      if (!uses)
		break;

	      df_link *use;
	      rtx dest2 = SET_DEST (*dest_sub_rtx2);
	      for (use = uses; use; use = use->next)
		if (paradoxical_subreg_p (GET_MODE (*DF_REF_LOC (use->ref)),
					  GET_MODE (dest2))
		    && !DEBUG_INSN_P (DF_REF_INSN (use->ref)))
		  break;
	      if (use)
		break;
	    }

	  /* The destination register of the extension insn must not be
	     used or set between the def_insn2 and def_insn exclusive.
	     Likewise for the other reg, i.e. check both reg1 and reg2
	     in the above comment.  */
	  if (reg_used_between_p (SET_DEST (set), def_insn2, def_insn)
	      || reg_set_between_p (SET_DEST (set), def_insn2, def_insn)
	      || reg_used_between_p (src_reg, def_insn2, def_insn)
	      || reg_set_between_p (src_reg, def_insn2, def_insn))
	    break;

	  state->defs_list[0] = def_insn2;
	  break;
	}
    }

  /* If cand->insn has been already modified, update cand->mode to a wider
     mode if possible, or punt.  */
  if (state->modified[INSN_UID (cand->insn)].kind != EXT_MODIFIED_NONE)
    {
      machine_mode mode;

      if (state->modified[INSN_UID (cand->insn)].kind
	  != (cand->code == ZERO_EXTEND
	      ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT)
	  || state->modified[INSN_UID (cand->insn)].mode != cand->mode
	  || (set == NULL_RTX))
	return false;
      mode = GET_MODE (SET_DEST (set));
      gcc_assert (GET_MODE_UNIT_SIZE (mode)
		  >= GET_MODE_UNIT_SIZE (cand->mode));
      cand->mode = mode;
    }

  merge_successful = true;

  /* Go through the defs vector and try to merge all the definitions
     in this vector.  */
  state->modified_list.truncate (0);
  FOR_EACH_VEC_ELT (state->defs_list, defs_ix, def_insn)
    {
      if (merge_def_and_ext (cand, def_insn, state))
	state->modified_list.safe_push (def_insn);
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
      FOR_EACH_VEC_ELT (state->copies_list, i, def_insn)
        {
          if (transform_ifelse (cand, def_insn))
	    state->modified_list.safe_push (def_insn);
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

	  FOR_EACH_VEC_ELT (state->modified_list, i, def_insn)
	    {
	      ext_modified *modified = &state->modified[INSN_UID (def_insn)];
	      if (modified->kind == EXT_MODIFIED_NONE)
		modified->kind = (cand->code == ZERO_EXTEND ? EXT_MODIFIED_ZEXT
						            : EXT_MODIFIED_SEXT);

	      if (copy_needed)
		modified->do_not_reextend = 1;
	    }
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
	      FOR_EACH_VEC_ELT (state->modified_list, i, def_insn)
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
add_removable_extension (const_rtx expr, rtx_insn *insn,
			 vec<ext_cand> *insn_list,
			 unsigned *def_map,
			 bitmap init_regs)
{
  enum rtx_code code;
  machine_mode mode;
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
      && REG_P (XEXP (src, 0)))
    {
      rtx reg = XEXP (src, 0);
      struct df_link *defs, *def;
      ext_cand *cand;

      if (fixed_regs[REGNO (reg)])
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Cannot eliminate extension:\n");
	      print_rtl_single (dump_file, insn);
	      fprintf (dump_file, " because extension on fixed register"
				  " isn't supported.\n");
	    }
	  return;
	}

      /* Zero-extension of an undefined value is partly defined (it's
	 completely undefined for sign-extension, though).  So if there exists
	 a path from the entry to this zero-extension that leaves this register
	 uninitialized, removing the extension could change the behavior of
	 correct programs.  So first, check it is not the case.  */
      if (code == ZERO_EXTEND && !bitmap_bit_p (init_regs, REGNO (reg)))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Cannot eliminate extension:\n");
	      print_rtl_single (dump_file, insn);
	      fprintf (dump_file, " because it can operate on uninitialized"
			          " data\n");
	    }
	  return;
	}

      /* Second, make sure we can get all the reaching definitions.  */
      defs = get_defs (insn, reg, NULL);
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

      /* Third, make sure the reaching definitions don't feed another and
	 different extension.  FIXME: this obviously can be improved.  */
      for (def = defs; def; def = def->next)
	if ((idx = def_map[INSN_UID (DF_REF_INSN (def->ref))])
	    && idx != -1U
	    && (cand = &(*insn_list)[idx - 1])
	    && cand->code != code)
	  {
	    if (dump_file)
	      {
	        fprintf (dump_file, "Cannot eliminate extension:\n");
		print_rtl_single (dump_file, insn);
	        fprintf (dump_file, " because of other extension\n");
	      }
	    return;
	  }
	/* For vector mode extensions, ensure that all uses of the
	   XEXP (src, 0) register are in insn or debug insns, as unlike
	   integral extensions lowpart subreg of the sign/zero extended
	   register are not equal to the original register, so we have
	   to change all uses or none and the current code isn't able
	   to change them all at once in one transaction.  */
	else if (VECTOR_MODE_P (GET_MODE (XEXP (src, 0))))
	  {
	    struct df_link *ref_chain, *ref_link;

	    ref_chain = DF_REF_CHAIN (def->ref);
	    for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
	      {
		if (ref_link->ref == NULL
		    || DF_REF_INSN_INFO (ref_link->ref) == NULL)
		  {
		    idx = -1U;
		    break;
		  }
		rtx_insn *use_insn = DF_REF_INSN (ref_link->ref);
		if (use_insn != insn && !DEBUG_INSN_P (use_insn))
		  {
		    idx = -1U;
		    break;
		  }
	      }

	    if (idx == -1U)
	      {
		def_map[INSN_UID (DF_REF_INSN (def->ref))] = idx;
		if (dump_file)
		  {
		    fprintf (dump_file, "Cannot eliminate extension:\n");
		    print_rtl_single (dump_file, insn);
		    fprintf (dump_file,
			     " because some vector uses aren't extension\n");
		  }
		return;
	      }
	  }

      /* Fourth, if the extended version occupies more registers than the
	 original and the source of the extension is the same hard register
	 as the destination of the extension, then we cannot eliminate
	 the extension without deep analysis, so just punt.

	 We allow this when the registers are different because the
	 code in combine_reaching_defs will handle that case correctly.  */
      if (hard_regno_nregs (REGNO (dest), mode) != REG_NREGS (reg)
	  && reg_overlap_mentioned_p (dest, reg))
	return;

      /* Then add the candidate to the list and insert the reaching definitions
         into the definition map.  */
      ext_cand e = {expr, code, mode, insn};
      insn_list->safe_push (e);
      idx = insn_list->length ();

      for (def = defs; def; def = def->next)
	def_map[INSN_UID (DF_REF_INSN (def->ref))] = idx;
    }
}

/* Traverse the instruction stream looking for extensions and return the
   list of candidates.  */

static vec<ext_cand>
find_removable_extensions (void)
{
  vec<ext_cand> insn_list = vNULL;
  basic_block bb;
  rtx_insn *insn;
  rtx set;
  unsigned *def_map = XCNEWVEC (unsigned, max_insn_uid);
  bitmap_head init, kill, gen, tmp;

  bitmap_initialize (&init, NULL);
  bitmap_initialize (&kill, NULL);
  bitmap_initialize (&gen, NULL);
  bitmap_initialize (&tmp, NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_copy (&init, DF_MIR_IN (bb));
      bitmap_clear (&kill);
      bitmap_clear (&gen);

      FOR_BB_INSNS (bb, insn)
	{
	  if (NONDEBUG_INSN_P (insn))
	    {
	      set = single_set (insn);
	      if (set != NULL_RTX)
		add_removable_extension (set, insn, &insn_list, def_map,
					 &init);
	      df_mir_simulate_one_insn (bb, insn, &kill, &gen);
	      bitmap_ior_and_compl (&tmp, &gen, &init, &kill);
	      bitmap_copy (&init, &tmp);
	    }
	}
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
  rtx_insn *curr_insn = NULL;
  int num_re_opportunities = 0, num_realized = 0, i;
  vec<ext_cand> reinsn_list;
  auto_vec<rtx_insn *> reinsn_del_list;
  auto_vec<rtx_insn *> reinsn_copy_list;

  /* Construct DU chain to get all reaching definitions of each
     extension instruction.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN + DF_DU_CHAIN);
  df_mir_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  max_insn_uid = get_max_uid ();
  reinsn_list = find_removable_extensions ();

  ext_state state;
  if (reinsn_list.is_empty ())
    state.modified = NULL;
  else
    state.modified = XCNEWVEC (struct ext_modified, max_insn_uid);

  FOR_EACH_VEC_ELT (reinsn_list, i, curr_cand)
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
	  /* If the RHS of the current candidate is not (extend (reg)), then
	     we do not allow the optimization of extensions where
	     the source and destination registers do not match.  Thus
	     checking REG_P here is correct.  */
	  rtx set = single_set (curr_cand->insn);
	  if (REG_P (XEXP (SET_SRC (set), 0))
	      && (REGNO (SET_DEST (set)) != REGNO (XEXP (SET_SRC (set), 0))))
	    {
              reinsn_copy_list.safe_push (curr_cand->insn);
              reinsn_copy_list.safe_push (state.defs_list[0]);
	    }
	  reinsn_del_list.safe_push (curr_cand->insn);
	  state.modified[INSN_UID (curr_cand->insn)].deleted = 1;
        }
    }

  /* The copy list contains pairs of insns which describe copies we
     need to insert into the INSN stream.

     The first insn in each pair is the extension insn, from which
     we derive the source and destination of the copy.

     The second insn in each pair is the memory reference where the
     extension will ultimately happen.  We emit the new copy
     immediately after this insn.

     It may first appear that the arguments for the copy are reversed.
     Remember that the memory reference will be changed to refer to the
     destination of the extention.  So we're actually emitting a copy
     from the new destination to the old destination.  */
  for (unsigned int i = 0; i < reinsn_copy_list.length (); i += 2)
    {
      rtx_insn *curr_insn = reinsn_copy_list[i];
      rtx_insn *def_insn = reinsn_copy_list[i + 1];

      /* Use the mode of the destination of the defining insn
	 for the mode of the copy.  This is necessary if the
	 defining insn was used to eliminate a second extension
	 that was wider than the first.  */
      rtx sub_rtx = *get_sub_rtx (def_insn);
      rtx set = single_set (curr_insn);
      rtx new_dst = gen_rtx_REG (GET_MODE (SET_DEST (sub_rtx)),
				 REGNO (XEXP (SET_SRC (set), 0)));
      rtx new_src = gen_rtx_REG (GET_MODE (SET_DEST (sub_rtx)),
				 REGNO (SET_DEST (set)));
      rtx new_set = gen_rtx_SET (new_dst, new_src);
      emit_insn_after (new_set, def_insn);
    }

  /* Delete all useless extensions here in one sweep.  */
  FOR_EACH_VEC_ELT (reinsn_del_list, i, curr_insn)
    delete_insn (curr_insn);

  reinsn_list.release ();
  XDELETEVEC (state.modified);

  if (dump_file && num_re_opportunities > 0)
    fprintf (dump_file, "Elimination opportunities = %d realized = %d\n",
	     num_re_opportunities, num_realized);
}

/* Find and remove redundant extensions.  */

static unsigned int
rest_of_handle_ree (void)
{
  find_and_remove_re ();
  return 0;
}

namespace {

const pass_data pass_data_ree =
{
  RTL_PASS, /* type */
  "ree", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ree : public rtl_opt_pass
{
public:
  pass_ree (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_ree, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return (optimize > 0 && flag_ree); }
  unsigned int execute (function *) final override
  {
    return rest_of_handle_ree ();
  }

}; // class pass_ree

} // anon namespace

rtl_opt_pass *
make_pass_ree (gcc::context *ctxt)
{
  return new pass_ree (ctxt);
}
