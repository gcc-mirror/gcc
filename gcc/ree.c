/* Redundant Extension Elimination pass for the GNU compiler.
  Copyright (C) 2010-2011 Free Software Foundation, Inc.
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
   Otherwise, eax may not be zero-extended.  Consider conditional move as
   RTL instructions of the form
   (set (reg:SI x) (if_then_else (cond) (reg:SI y) (reg:SI z))).
   This pass tries to merge an extension with a conditional move by
   actually merging the defintions of y and z with an extension and then
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
     40031d:       89 ff                   mov    %edi,%edi  --> Useless extend
     40031f:       8b 04 bd 60 19 40 00    mov    0x401960(,%rdi,4),%eax
     400326:       c3                      retq
     ......
     400330:       ba 2d 00 00 00          mov    $0x2d,%edx
     400335:       0f af fa                imul   %edx,%edi
     400338:       89 ff                   mov    %edi,%edi  --> Useless extend
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
     40036d:       89 c0                   mov    %eax,%eax  --> Useless extend
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
    1b:   0f b6 c9                movzbl %cl,%ecx  --> Useless extend
    1e:   0f b6 c0                movzbl %al,%eax  --> Useless extend
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

/* This says if a register is newly created for the purpose of
   extension.  */

enum insn_merge_code
{
  MERGE_NOT_ATTEMPTED = 0,
  MERGE_SUCCESS
};

/* This structure is used to hold data about candidate for
   elimination.  */

typedef struct GTY(()) ext_cand
{
  rtx insn;
  const_rtx expr;
  enum machine_mode src_mode;
} ext_cand, *ext_cand_ref;

DEF_VEC_O(ext_cand);
DEF_VEC_ALLOC_O(ext_cand, heap);

/* This says if a INSN UID or its definition has already been merged
   with a extension or not.  */

static enum insn_merge_code *is_insn_merge_attempted;
static int max_insn_uid;

/* Return the merge code status for INSN.  */

static enum insn_merge_code
get_insn_status (rtx insn)
{
  gcc_assert (INSN_UID (insn) < max_insn_uid);
  return is_insn_merge_attempted[INSN_UID (insn)];
}

/* Set the merge code status of INSN to CODE.  */

static void
set_insn_status (rtx insn, enum insn_merge_code code)
{
  gcc_assert (INSN_UID (insn) < max_insn_uid);
  is_insn_merge_attempted[INSN_UID (insn)] = code;
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
   (set (reg a) (extend (expression)))

   Special Cases :
   If the expression is a constant or another extend directly
   assign it to the register.  */

static bool
combine_set_extend (ext_cand_ref cand, rtx curr_insn, rtx *orig_set)
{
  rtx temp_extension, simplified_temp_extension, new_set, new_const_int;
  rtx orig_src, cand_src;
  rtx newreg;
  enum machine_mode dst_mode = GET_MODE (SET_DEST (cand->expr));

  /* Change the SET rtx and validate it.  */
  orig_src = SET_SRC (*orig_set);
  cand_src = SET_SRC (cand->expr);
  new_set = NULL_RTX;

  newreg = gen_rtx_REG (dst_mode, REGNO (SET_DEST (*orig_set)));

  /* Merge constants by directly moving the constant into the
     register under some conditions.  */

  if (GET_CODE (orig_src) == CONST_INT
      && HOST_BITS_PER_WIDE_INT >= GET_MODE_BITSIZE (dst_mode))
    {
      if (INTVAL (orig_src) >= 0 || GET_CODE (cand_src) == SIGN_EXTEND)
	new_set = gen_rtx_SET (VOIDmode, newreg, orig_src);
      else
	{
	  /* Zero-extend the negative constant by masking out the bits outside
	     the source mode.  */
	  enum machine_mode src_mode = GET_MODE (SET_DEST (*orig_set));
	  new_const_int
	    = GEN_INT (INTVAL (orig_src) & GET_MODE_MASK (src_mode));
	  new_set = gen_rtx_SET (VOIDmode, newreg, new_const_int);
	}
    }
  else if (GET_MODE (orig_src) == VOIDmode)
    {
      /* This is mostly due to a call insn that should not be
	 optimized.  */

      return false;
    }
  else if (GET_CODE (orig_src) == GET_CODE (cand_src))
    {
      /* Here is a sequence of two extensions.  Try to merge them into a
	 single one.  */

      temp_extension
	= gen_rtx_fmt_e (GET_CODE (orig_src), dst_mode, XEXP (orig_src, 0));
      simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (VOIDmode, newreg, temp_extension);
    }
  else if (GET_CODE (orig_src) == IF_THEN_ELSE)
    {
      /* Only IF_THEN_ELSE of phi-type copies are combined.  Otherwise,
         in general, IF_THEN_ELSE should not be combined.  */

      return false;
    }
  else
    {
      /* This is the normal case we expect.  */

      temp_extension
	= gen_rtx_fmt_e (GET_CODE (cand_src), dst_mode, orig_src);
      simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (VOIDmode, newreg, temp_extension);
    }

  gcc_assert (new_set != NULL_RTX);

  /* This change is a part of a group of changes.  Hence,
     validate_change will not try to commit the change.  */

  if (validate_change (curr_insn, orig_set, new_set, true))
    {
      if (dump_file)
        {
          fprintf (dump_file, "Merged Instruction with EXTEND:\n");
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
transform_ifelse (ext_cand_ref cand, rtx def_insn)
{
  rtx set_insn = PATTERN (def_insn);
  rtx srcreg, dstreg, srcreg2;
  rtx map_srcreg, map_dstreg, map_srcreg2;
  rtx ifexpr;
  rtx cond;
  rtx new_set;
  enum machine_mode dst_mode = GET_MODE (SET_DEST (cand->expr));

  gcc_assert (GET_CODE (set_insn) == SET);
  cond = XEXP (SET_SRC (set_insn), 0);
  dstreg = SET_DEST (set_insn);
  srcreg = XEXP (SET_SRC (set_insn), 1);
  srcreg2 = XEXP (SET_SRC (set_insn), 2);
  map_srcreg = gen_rtx_REG (dst_mode, REGNO (srcreg));
  map_srcreg2 = gen_rtx_REG (dst_mode, REGNO (srcreg2));
  map_dstreg = gen_rtx_REG (dst_mode, REGNO (dstreg));
  ifexpr = gen_rtx_IF_THEN_ELSE (dst_mode, cond, map_srcreg, map_srcreg2);
  new_set = gen_rtx_SET (VOIDmode, map_dstreg, ifexpr);

  if (validate_change (def_insn, &PATTERN (def_insn), new_set, true))
    {
      if (dump_file)
        {
          fprintf (dump_file, "Cond_Move Instruction's mode extended :\n");
          print_rtl_single (dump_file, def_insn);
        }
      return true;
    }
  else
    return false;
}

/* Function to get all the immediate definitions of an instruction.
   The reaching definitions are desired for WHICH_REG used in
   CURR_INSN.  This function returns 0 if there was an error getting
   a definition.  Upon success, this function returns the number of
   definitions and stores the definitions in DEST.  */

static int
get_defs (rtx curr_insn, rtx which_reg, VEC (rtx,heap) **dest)
{
  df_ref reg_info, *defs;
  struct df_link *def_chain;
  int n_refs = 0;

  defs = DF_INSN_USES (curr_insn);
  reg_info = NULL;

  while (*defs)
    {
      reg_info = *defs;
      if (GET_CODE (DF_REF_REG (reg_info)) == SUBREG)
        return 0;
      if (REGNO (DF_REF_REG (reg_info)) == REGNO (which_reg))
        break;
      defs++;
    }

  gcc_assert (reg_info != NULL && defs != NULL);
  def_chain = DF_REF_CHAIN (reg_info);

  while (def_chain)
    {
      /* Problem getting some definition for this instruction.  */

      if (def_chain->ref == NULL)
        return 0;
      if (DF_REF_INSN_INFO (def_chain->ref) == NULL)
        return 0;
      def_chain = def_chain->next;
    }

  def_chain = DF_REF_CHAIN (reg_info);

  if (dest == NULL)
    return 1;

  while (def_chain)
    {
      VEC_safe_push (rtx, heap, *dest, DF_REF_INSN (def_chain->ref));
      def_chain = def_chain->next;
      n_refs++;
    }
  return n_refs;
}

/* rtx function to check if this SET insn, EXPR, is a conditional copy insn :
   (set (reg a ) (IF_THEN_ELSE (cond) (reg b) (reg c)))
   Called from is_insn_cond_copy.  DATA stores the two registers on each
   side of the condition.  */

static int
is_this_a_cmove (rtx expr, void *data)
{
  /* Check for conditional (if-then-else) copy.  */

  if (GET_CODE (expr) == SET
      && GET_CODE (SET_DEST (expr)) == REG
      && GET_CODE (SET_SRC (expr))  == IF_THEN_ELSE
      && GET_CODE (XEXP (SET_SRC (expr), 1)) == REG
      && GET_CODE (XEXP (SET_SRC (expr), 2)) == REG)
    {
      ((rtx *)data)[0] = XEXP (SET_SRC (expr), 1);
      ((rtx *)data)[1] = XEXP (SET_SRC (expr), 2);
      return 1;
    }
  return 0;
}

/* This returns 1 if it found
   (SET (reg REGNO (def_reg)) (if_then_else (cond) (REG x1) (REG x2)))
   in the DEF_INSN pattern.  It stores the x1 and x2 in COPY_REG_1
   and COPY_REG_2.  */

static int
is_insn_cond_copy (rtx def_insn, rtx *copy_reg_1, rtx *copy_reg_2)
{
  int type;
  rtx set_expr;
  rtx srcreg[2];

  srcreg[0] = NULL_RTX;
  srcreg[1] = NULL_RTX;

  set_expr = single_set (def_insn);

  if (set_expr == NULL_RTX)
    return 0;

  type = is_this_a_cmove (set_expr, (void *) srcreg);

  if (type)
    {
      *copy_reg_1 = srcreg[0];
      *copy_reg_2 = srcreg[1];
      return type;
    }

  return 0;
}

/* Reaching Definitions of the extended register could be conditional copies
   or regular definitions.  This function separates the two types into two
   lists, DEFS_LIST and COPIES_LIST.  This is necessary because, if a reaching
   definition is a conditional copy, combining the extend with this definition
   is wrong.  Conditional copies are merged by transitively merging its
   definitions.  The defs_list is populated with all the reaching definitions
   of the extension instruction (EXTEND_INSN) which must be merged with an
   extension.  The copies_list contains all the conditional moves that will
   later be extended into a wider mode conditonal move if all the merges are
   successful.  The function returns false when there is a failure in getting
   some definitions, like that of parameters.  It returns 1 upon success, 0
   upon failure and 2 when all definitions of the EXTEND_INSN were merged
   previously.  */

static int
make_defs_and_copies_lists (rtx extend_insn, rtx set_pat,
                            VEC (rtx,heap) **defs_list,
                            VEC (rtx,heap) **copies_list)
{
  bool *is_insn_visited;
  VEC (rtx,heap) *work_list;
  rtx srcreg, copy_reg_1, copy_reg_2;
  rtx def_insn;
  int n_defs = 0;
  int vec_index = 0;
  int n_worklist = 0;
  int i, is_copy;

  srcreg = XEXP (SET_SRC (set_pat), 0);
  work_list = VEC_alloc (rtx, heap, 8);

  /* Initialize the Work List */
  n_worklist = get_defs (extend_insn, srcreg, &work_list);

  if (n_worklist == 0)
    {
      VEC_free (rtx, heap, work_list);
      /* The number of defs being equal to zero can only imply that all of its
         definitions have been previously merged.  */
      return 2;
    }

  is_insn_visited = XNEWVEC (bool, max_insn_uid);

  for (i = 0; i < max_insn_uid; i++)
    is_insn_visited[i] = false;


  /* Perform transitive closure for conditional copies.  */
  while (n_worklist > vec_index)
    {
      def_insn = VEC_index (rtx, work_list, vec_index);
      gcc_assert (INSN_UID (def_insn) < max_insn_uid);

      if (is_insn_visited[INSN_UID (def_insn)])
        {
          vec_index++;
          continue;
        }

      is_insn_visited[INSN_UID (def_insn)] = true;
      copy_reg_1 = copy_reg_2 = NULL_RTX;
      is_copy = is_insn_cond_copy (def_insn, &copy_reg_1, &copy_reg_2);
      if (is_copy)
        {
          gcc_assert (copy_reg_1 && copy_reg_2);

          /* Push it into the copy list first.  */

          VEC_safe_push (rtx, heap, *copies_list, def_insn);

          /* Perform transitive closure here */

          n_defs = get_defs (def_insn, copy_reg_1, &work_list);

          if (n_defs == 0)
            {
              VEC_free (rtx, heap, work_list);
              XDELETEVEC (is_insn_visited);
              return 0;
            }
          n_worklist += n_defs;

          n_defs = get_defs (def_insn, copy_reg_2, &work_list);
          if (n_defs == 0)
            {
              VEC_free (rtx, heap, work_list);
              XDELETEVEC (is_insn_visited);
              return 0;
            }
          n_worklist += n_defs;
        }
      else
        {
          VEC_safe_push (rtx, heap, *defs_list, def_insn);
        }
      vec_index++;
    }

  VEC_free (rtx, heap, work_list);
  XDELETEVEC (is_insn_visited);
  return 1;
}

/* Merge the DEF_INSN with an extension.  Calls combine_set_extend
   on the SET pattern.  */

static bool
merge_def_and_ext (ext_cand_ref cand, rtx def_insn)
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

  if (GET_CODE (SET_DEST (*sub_rtx)) == REG
      && GET_MODE (SET_DEST (*sub_rtx)) == ext_src_mode)
    {
      return combine_set_extend (cand, def_insn, sub_rtx);
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
combine_reaching_defs (ext_cand_ref cand, rtx set_pat)
{
  rtx def_insn;
  bool merge_successful = true;
  int i;
  int defs_ix;
  int outcome;

  /* To store the definitions that have been merged.  */

  VEC (rtx, heap) *defs_list, *copies_list, *vec;
  enum insn_merge_code merge_code;

  defs_list = VEC_alloc (rtx, heap, 8);
  copies_list = VEC_alloc (rtx, heap, 8);

  outcome = make_defs_and_copies_lists (cand->insn,
                                        set_pat, &defs_list, &copies_list);

  /* outcome == 2 implies that all the definitions for this extension were
     merged while previously when handling other extension.  */

  if (outcome == 2)
    {
      VEC_free (rtx, heap, defs_list);
      VEC_free (rtx, heap, copies_list);
      if (dump_file)
        fprintf (dump_file, "All definitions have been merged previously.\n");
      return true;
    }

  if (outcome == 0)
    {
      VEC_free (rtx, heap, defs_list);
      VEC_free (rtx, heap, copies_list);
      return false;
    }

  merge_successful = true;

  /* Go through the defs vector and try to merge all the definitions
     in this vector.  */

  vec = VEC_alloc (rtx, heap, 8);
  FOR_EACH_VEC_ELT (rtx, defs_list, defs_ix, def_insn)
    {
      merge_code = get_insn_status (def_insn);
      gcc_assert (merge_code == MERGE_NOT_ATTEMPTED);

      if (merge_def_and_ext (cand, def_insn))
        VEC_safe_push (rtx, heap, vec, def_insn);
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
      FOR_EACH_VEC_ELT (rtx, copies_list, i, def_insn)
        {
          if (transform_ifelse (cand, def_insn))
            {
              VEC_safe_push (rtx, heap, vec, def_insn);
            }
          else
            {
              merge_successful = false;
              break;
            }
        }
    }

  if (merge_successful)
    {
      /* Commit the changes here if possible */
      /* XXX : Now, it is an all or nothing scenario.  Even if one definition
         cannot be merged we totally fail.  In future, allow extensions to
         be partially eliminated along those paths where the definitions could
         be merged.  */

      if (apply_change_group ())
        {
          if (dump_file)
            fprintf (dump_file, "All merges were successful ....\n");

          FOR_EACH_VEC_ELT (rtx, vec, i, def_insn)
            {
              set_insn_status (def_insn, MERGE_SUCCESS);
            }

          VEC_free (rtx, heap, vec);
          VEC_free (rtx, heap, defs_list);
          VEC_free (rtx, heap, copies_list);
          return true;
        }
      else
        {
          /* Changes need not be cancelled explicitly as apply_change_group
             does it.  Print list of definitions in the dump_file for debug
             purposes.  This extension cannot be deleted.  */

          if (dump_file)
            {
              FOR_EACH_VEC_ELT (rtx, vec, i, def_insn)
                {
                  fprintf (dump_file, " Ummergable definitions : \n");
                  print_rtl_single (dump_file, def_insn);
                }
            }
        }
    }
  else
    {
      /* Cancel any changes that have been made so far.  */
      cancel_changes (0);
    }

  VEC_free (rtx, heap, vec);
  VEC_free (rtx, heap, defs_list);
  VEC_free (rtx, heap, copies_list);
  return false;
}

/* Carry information about extensions while walking the RTL.  */

struct extend_info
{
  /* The insn where the extension is.  */
  rtx insn;

  /* The list of candidates.  */
  VEC (ext_cand, heap) *insn_list;
};

static void
add_ext_candidate (VEC (ext_cand, heap) **exts,
		   rtx insn, const_rtx expr)
{
  ext_cand_ref ec = VEC_safe_push (ext_cand, heap, *exts, NULL);
  ec->insn = insn;
  ec->expr = expr;
}

/* Add an extension pattern that could be eliminated.  This is called via
   note_stores from find_removable_extensions.  */

static void
add_removable_extension (rtx x ATTRIBUTE_UNUSED, const_rtx expr, void *data)
{
  struct extend_info *rei = (struct extend_info *)data;
  rtx src, dest;

  /* We are looking for SET (REG N) (EXTEND (REG N)).  */
  if (GET_CODE (expr) != SET)
    return;

  src = SET_SRC (expr);
  dest = SET_DEST (expr);

  if (REG_P (dest)
      && (GET_CODE (src) == ZERO_EXTEND || GET_CODE (src) == SIGN_EXTEND)
      && REG_P (XEXP (src, 0))
      && REGNO (dest) == REGNO (XEXP (src, 0)))
    {
      if (get_defs (rei->insn, XEXP (src, 0), NULL))
	add_ext_candidate (&rei->insn_list, rei->insn, expr);
      else if (dump_file)
	{
	  fprintf (dump_file, "Cannot eliminate extension: \n");
	  print_rtl_single (dump_file, rei->insn);
	  fprintf (dump_file, "No defs. Could be extending parameters.\n");
	}
    }
}

/* Traverse the instruction stream looking for extensions and return the
   list of candidates.  */

static VEC (ext_cand, heap)*
find_removable_extensions (void)
{
  struct extend_info rei;
  basic_block bb;
  rtx insn;

  rei.insn_list = VEC_alloc (ext_cand, heap, 8);

  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
      {
	if (!NONDEBUG_INSN_P (insn))
	  continue;

	rei.insn = insn;
	note_stores (PATTERN (insn), add_removable_extension, &rei);
      }

  return rei.insn_list;
}

/* This is the main function that checks the insn stream for redundant
   extensions and tries to remove them if possible.  */

static unsigned int
find_and_remove_re (void)
{
  ext_cand_ref curr_cand;
  rtx curr_insn = NULL_RTX;
  int i;
  int ix;
  long num_realized = 0;
  long num_re_opportunities = 0;
  VEC (ext_cand, heap) *reinsn_list;
  VEC (rtx, heap) *reinsn_del_list;

  /* Construct DU chain to get all reaching definitions of each
     extension instruction.  */

  df_chain_add_problem (DF_UD_CHAIN + DF_DU_CHAIN);
  df_analyze ();

  max_insn_uid = get_max_uid ();

  is_insn_merge_attempted
    = XNEWVEC (enum insn_merge_code,
	       sizeof (enum insn_merge_code) * max_insn_uid);

  for (i = 0; i < max_insn_uid; i++)
    is_insn_merge_attempted[i] = MERGE_NOT_ATTEMPTED;

  num_re_opportunities = num_realized = 0;

  reinsn_del_list = VEC_alloc (rtx, heap, 4);

  reinsn_list = find_removable_extensions ();

  FOR_EACH_VEC_ELT (ext_cand, reinsn_list, ix, curr_cand)
    {
      num_re_opportunities++;
      /* Try to combine the extension with the definition here.  */

      if (dump_file)
        {
          fprintf (dump_file, "Trying to eliminate extension : \n");
          print_rtl_single (dump_file, curr_insn);
        }

      if (combine_reaching_defs (curr_cand, PATTERN (curr_cand->insn)))
        {
          if (dump_file)
            fprintf (dump_file, "Eliminated the extension...\n");
          num_realized++;
          VEC_safe_push (rtx, heap, reinsn_del_list, curr_cand->insn);
        }
    }

  /* Delete all useless extensions here in one sweep.  */
  FOR_EACH_VEC_ELT (rtx, reinsn_del_list, ix, curr_insn)
    delete_insn (curr_insn);

  free (is_insn_merge_attempted);
  VEC_free (ext_cand, heap, reinsn_list);
  VEC_free (rtx, heap, reinsn_del_list);

  if (dump_file && num_re_opportunities > 0)
    fprintf (dump_file, "\n %s : num_re_opportunities = %ld "
                        "num_realized = %ld \n",
                        current_function_name (),
                        num_re_opportunities, num_realized);

  df_finish_pass (false);
  return 0;
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
