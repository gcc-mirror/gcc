/* Scheduler hooks for IA-32 which implement bdver1-4 specific logic.
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "tm_p.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "target.h"
#include "rtl-iter.h"
#include "regset.h"
#include "sched-int.h"

/* The size of the dispatch window is the total number of bytes of
   object code allowed in a window.  */
#define DISPATCH_WINDOW_SIZE 16

/* Number of dispatch windows considered for scheduling.  */
#define MAX_DISPATCH_WINDOWS 3

/* Maximum number of instructions in a window.  */
#define MAX_INSN 4

/* Maximum number of immediate operands in a window.  */
#define MAX_IMM 4

/* Maximum number of immediate bits allowed in a window.  */
#define MAX_IMM_SIZE 128

/* Maximum number of 32 bit immediates allowed in a window.  */
#define MAX_IMM_32 4

/* Maximum number of 64 bit immediates allowed in a window.  */
#define MAX_IMM_64 2

/* Maximum total of loads or prefetches allowed in a window.  */
#define MAX_LOAD 2

/* Maximum total of stores allowed in a window.  */
#define MAX_STORE 1

#undef BIG
#define BIG 100


/* Dispatch groups.  Istructions that affect the mix in a dispatch window.  */
enum dispatch_group {
  disp_no_group = 0,
  disp_load,
  disp_store,
  disp_load_store,
  disp_prefetch,
  disp_imm,
  disp_imm_32,
  disp_imm_64,
  disp_branch,
  disp_cmp,
  disp_jcc,
  disp_last
};

/* Number of allowable groups in a dispatch window.  It is an array
   indexed by dispatch_group enum.  100 is used as a big number,
   because the number of these kind of operations does not have any
   effect in dispatch window, but we need them for other reasons in
   the table.  */
static unsigned int num_allowable_groups[disp_last] = {
  0, 2, 1, 1, 2, 4, 4, 2, 1, BIG, BIG
};

char group_name[disp_last + 1][16] = {
  "disp_no_group", "disp_load", "disp_store", "disp_load_store",
  "disp_prefetch", "disp_imm", "disp_imm_32", "disp_imm_64",
  "disp_branch", "disp_cmp", "disp_jcc", "disp_last"
};

/* Instruction path.  */
enum insn_path {
  no_path = 0,
  path_single, /* Single micro op.  */
  path_double, /* Double micro op.  */
  path_multi,  /* Instructions with more than 2 micro op..  */
  last_path
};

/* sched_insn_info defines a window to the instructions scheduled in
   the basic block.  It contains a pointer to the insn_info table and
   the instruction scheduled.

   Windows are allocated for each basic block and are linked
   together.  */
typedef struct sched_insn_info_s {
  rtx insn;
  enum dispatch_group group;
  enum insn_path path;
  int byte_len;
  int imm_bytes;
} sched_insn_info;

/* Linked list of dispatch windows.  This is a two way list of
   dispatch windows of a basic block.  It contains information about
   the number of uops in the window and the total number of
   instructions and of bytes in the object code for this dispatch
   window.  */
typedef struct dispatch_windows_s {
  int num_insn;            /* Number of insn in the window.  */
  int num_uops;            /* Number of uops in the window.  */
  int window_size;         /* Number of bytes in the window.  */
  int window_num;          /* Window number between 0 or 1.  */
  int num_imm;             /* Number of immediates in an insn.  */
  int num_imm_32;          /* Number of 32 bit immediates in an insn.  */
  int num_imm_64;          /* Number of 64 bit immediates in an insn.  */
  int imm_size;            /* Total immediates in the window.  */
  int num_loads;           /* Total memory loads in the window.  */
  int num_stores;          /* Total memory stores in the window.  */
  int violation;          /* Violation exists in window.  */
  sched_insn_info *window; /* Pointer to the window.  */
  struct dispatch_windows_s *next;
  struct dispatch_windows_s *prev;
} dispatch_windows;

/* Immediate valuse used in an insn.  */
typedef struct imm_info_s
  {
    int imm;
    int imm32;
    int imm64;
  } imm_info;

static dispatch_windows *dispatch_window_list;
static dispatch_windows *dispatch_window_list1;

/* Get dispatch group of insn.  */

static enum dispatch_group
get_mem_group (rtx_insn *insn)
{
  enum attr_memory memory;

  if (INSN_CODE (insn) < 0)
    return disp_no_group;
  memory = get_attr_memory (insn);
  if (memory == MEMORY_STORE)
    return disp_store;

  if (memory == MEMORY_LOAD)
    return disp_load;

  if (memory == MEMORY_BOTH)
    return disp_load_store;

  return disp_no_group;
}

/* Return true if insn is a compare instruction.  */

static bool
is_cmp (rtx_insn *insn)
{
  enum attr_type type;

  type = get_attr_type (insn);
  return (type == TYPE_TEST
	  || type == TYPE_ICMP
	  || type == TYPE_FCMP
	  || GET_CODE (PATTERN (insn)) == COMPARE);
}

/* Return true if a dispatch violation encountered.  */

static bool
dispatch_violation (void)
{
  if (dispatch_window_list->next)
    return dispatch_window_list->next->violation;
  return dispatch_window_list->violation;
}

/* Return true if insn is a branch instruction.  */

static bool
is_branch (rtx_insn *insn)
{
  return (CALL_P (insn) || JUMP_P (insn));
}

/* Return true if insn is a prefetch instruction.  */

static bool
is_prefetch (rtx_insn *insn)
{
  return NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == PREFETCH;
}

/* This function initializes a dispatch window and the list container holding a
   pointer to the window.  */

static void
init_window (int window_num)
{
  int i;
  dispatch_windows *new_list;

  if (window_num == 0)
    new_list = dispatch_window_list;
  else
    new_list = dispatch_window_list1;

  new_list->num_insn = 0;
  new_list->num_uops = 0;
  new_list->window_size = 0;
  new_list->next = NULL;
  new_list->prev = NULL;
  new_list->window_num = window_num;
  new_list->num_imm = 0;
  new_list->num_imm_32 = 0;
  new_list->num_imm_64 = 0;
  new_list->imm_size = 0;
  new_list->num_loads = 0;
  new_list->num_stores = 0;
  new_list->violation = false;

  for (i = 0; i < MAX_INSN; i++)
    {
      new_list->window[i].insn = NULL;
      new_list->window[i].group = disp_no_group;
      new_list->window[i].path = no_path;
      new_list->window[i].byte_len = 0;
      new_list->window[i].imm_bytes = 0;
    }
  return;
}

/* This function allocates and initializes a dispatch window and the
   list container holding a pointer to the window.  */

static dispatch_windows *
allocate_window (void)
{
  dispatch_windows *new_list = XNEW (struct dispatch_windows_s);
  new_list->window = XNEWVEC (struct sched_insn_info_s, MAX_INSN + 1);

  return new_list;
}

/* This routine initializes the dispatch scheduling information.  It
   initiates building dispatch scheduler tables and constructs the
   first dispatch window.  */

static void
init_dispatch_sched (void)
{
  /* Allocate a dispatch list and a window.  */
  dispatch_window_list = allocate_window ();
  dispatch_window_list1 = allocate_window ();
  init_window (0);
  init_window (1);
}

/* This function returns true if a branch is detected.  End of a basic block
   does not have to be a branch, but here we assume only branches end a
   window.  */

static bool
is_end_basic_block (enum dispatch_group group)
{
  return group == disp_branch;
}

/* This function is called when the end of a window processing is reached.  */

static void
process_end_window (void)
{
  gcc_assert (dispatch_window_list->num_insn <= MAX_INSN);
  if (dispatch_window_list->next)
    {
      gcc_assert (dispatch_window_list1->num_insn <= MAX_INSN);
      gcc_assert (dispatch_window_list->window_size
		  + dispatch_window_list1->window_size <= 48);
      init_window (1);
    }
  init_window (0);
}

/* Allocates a new dispatch window and adds it to WINDOW_LIST.
   WINDOW_NUM is either 0 or 1.  A maximum of two windows are generated
   for 48 bytes of instructions.  Note that these windows are not dispatch
   windows that their sizes are DISPATCH_WINDOW_SIZE.  */

static dispatch_windows *
allocate_next_window (int window_num)
{
  if (window_num == 0)
    {
      if (dispatch_window_list->next)
	  init_window (1);
      init_window (0);
      return dispatch_window_list;
    }

  dispatch_window_list->next = dispatch_window_list1;
  dispatch_window_list1->prev = dispatch_window_list;

  return dispatch_window_list1;
}

/* Compute number of immediate operands of an instruction.  */

static void
find_constant (rtx in_rtx, imm_info *imm_values)
{
  if (INSN_P (in_rtx))
    in_rtx = PATTERN (in_rtx);
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, in_rtx, ALL)
    if (const_rtx x = *iter)
      switch (GET_CODE (x))
	{
	case CONST:
	case SYMBOL_REF:
	case CONST_INT:
	  (imm_values->imm)++;
	  if (x86_64_immediate_operand (CONST_CAST_RTX (x), SImode))
	    (imm_values->imm32)++;
	  else
	    (imm_values->imm64)++;
	  break;

	case CONST_DOUBLE:
	case CONST_WIDE_INT:
	  (imm_values->imm)++;
	  (imm_values->imm64)++;
	  break;

	case CODE_LABEL:
	  if (LABEL_KIND (x) == LABEL_NORMAL)
	    {
	      (imm_values->imm)++;
	      (imm_values->imm32)++;
	    }
	  break;

	default:
	  break;
	}
}

/* Return total size of immediate operands of an instruction along with number
   of corresponding immediate-operands.  It initializes its parameters to zero
   befor calling FIND_CONSTANT.
   INSN is the input instruction.  IMM is the total of immediates.
   IMM32 is the number of 32 bit immediates.  IMM64 is the number of 64
   bit immediates.  */

static int
get_num_immediates (rtx_insn *insn, int *imm, int *imm32, int *imm64)
{
  imm_info imm_values = {0, 0, 0};

  find_constant (insn, &imm_values);
  *imm = imm_values.imm;
  *imm32 = imm_values.imm32;
  *imm64 = imm_values.imm64;
  return imm_values.imm32 * 4 + imm_values.imm64 * 8;
}

/* This function indicates if an operand of an instruction is an
   immediate.  */

static bool
has_immediate (rtx_insn *insn)
{
  int num_imm_operand;
  int num_imm32_operand;
  int num_imm64_operand;

  if (insn)
    return get_num_immediates (insn, &num_imm_operand, &num_imm32_operand,
			       &num_imm64_operand);
  return false;
}

/* Return single or double path for instructions.  */

static enum insn_path
get_insn_path (rtx_insn *insn)
{
  enum attr_amdfam10_decode path = get_attr_amdfam10_decode (insn);

  if ((int)path == 0)
    return path_single;

  if ((int)path == 1)
    return path_double;

  return path_multi;
}

/* Return insn dispatch group.  */

static enum dispatch_group
get_insn_group (rtx_insn *insn)
{
  enum dispatch_group group = get_mem_group (insn);
  if (group)
    return group;

  if (is_branch (insn))
    return disp_branch;

  if (is_cmp (insn))
    return disp_cmp;

  if (has_immediate (insn))
    return disp_imm;

  if (is_prefetch (insn))
    return disp_prefetch;

  return disp_no_group;
}

/* Count number of GROUP restricted instructions in a dispatch
   window WINDOW_LIST.  */

static int
count_num_restricted (rtx_insn *insn, dispatch_windows *window_list)
{
  enum dispatch_group group = get_insn_group (insn);
  int imm_size;
  int num_imm_operand;
  int num_imm32_operand;
  int num_imm64_operand;

  if (group == disp_no_group)
    return 0;

  if (group == disp_imm)
    {
      imm_size = get_num_immediates (insn, &num_imm_operand, &num_imm32_operand,
			      &num_imm64_operand);
      if (window_list->imm_size + imm_size > MAX_IMM_SIZE
	  || num_imm_operand + window_list->num_imm > MAX_IMM
	  || (num_imm32_operand > 0
	      && (window_list->num_imm_32 + num_imm32_operand > MAX_IMM_32
		  || window_list->num_imm_64 * 2 + num_imm32_operand > MAX_IMM_32))
	  || (num_imm64_operand > 0
	      && (window_list->num_imm_64 + num_imm64_operand > MAX_IMM_64
		  || window_list->num_imm_32 + num_imm64_operand * 2 > MAX_IMM_32))
	  || (window_list->imm_size + imm_size == MAX_IMM_SIZE
	      && num_imm64_operand > 0
	      && ((window_list->num_imm_64 > 0
		   && window_list->num_insn >= 2)
		  || window_list->num_insn >= 3)))
	return BIG;

      return 1;
    }

  if ((group == disp_load_store
       && (window_list->num_loads >= MAX_LOAD
	   || window_list->num_stores >= MAX_STORE))
      || ((group == disp_load
	   || group == disp_prefetch)
	  && window_list->num_loads >= MAX_LOAD)
      || (group == disp_store
	  && window_list->num_stores >= MAX_STORE))
    return BIG;

  return 1;
}

/* This function returns true if insn satisfies dispatch rules on the
   last window scheduled.  */

static bool
fits_dispatch_window (rtx_insn *insn)
{
  dispatch_windows *window_list = dispatch_window_list;
  dispatch_windows *window_list_next = dispatch_window_list->next;
  unsigned int num_restrict;
  enum dispatch_group group = get_insn_group (insn);
  enum insn_path path = get_insn_path (insn);
  int sum;

  /* Make disp_cmp and disp_jcc get scheduled at the latest.  These
     instructions should be given the lowest priority in the
     scheduling process in Haifa scheduler to make sure they will be
     scheduled in the same dispatch window as the reference to them.  */
  if (group == disp_jcc || group == disp_cmp)
    return false;

  /* Check nonrestricted.  */
  if (group == disp_no_group || group == disp_branch)
    return true;

  /* Get last dispatch window.  */
  if (window_list_next)
    window_list = window_list_next;

  if (window_list->window_num == 1)
    {
      sum = window_list->prev->window_size + window_list->window_size;

      if (sum == 32
	  || (ix86_min_insn_size (insn) + sum) >= 48)
	/* Window 1 is full.  Go for next window.  */
	return true;
    }

  num_restrict = count_num_restricted (insn, window_list);

  if (num_restrict > num_allowable_groups[group])
    return false;

  /* See if it fits in the first window.  */
  if (window_list->window_num == 0)
    {
      /* The first widow should have only single and double path
	 uops.  */
      if (path == path_double
	  && (window_list->num_uops + 2) > MAX_INSN)
	return false;
      else if (path != path_single)
        return false;
    }
  return true;
}

/* Add an instruction INSN with NUM_UOPS micro-operations to the
   dispatch window WINDOW_LIST.  */

static void
add_insn_window (rtx_insn *insn, dispatch_windows *window_list, int num_uops)
{
  int byte_len = ix86_min_insn_size (insn);
  int num_insn = window_list->num_insn;
  int imm_size;
  sched_insn_info *window = window_list->window;
  enum dispatch_group group = get_insn_group (insn);
  enum insn_path path = get_insn_path (insn);
  int num_imm_operand;
  int num_imm32_operand;
  int num_imm64_operand;

  if (!window_list->violation && group != disp_cmp
      && !fits_dispatch_window (insn))
    window_list->violation = true;

  imm_size = get_num_immediates (insn, &num_imm_operand, &num_imm32_operand,
				 &num_imm64_operand);

  /* Initialize window with new instruction.  */
  window[num_insn].insn = insn;
  window[num_insn].byte_len = byte_len;
  window[num_insn].group = group;
  window[num_insn].path = path;
  window[num_insn].imm_bytes = imm_size;

  window_list->window_size += byte_len;
  window_list->num_insn = num_insn + 1;
  window_list->num_uops = window_list->num_uops + num_uops;
  window_list->imm_size += imm_size;
  window_list->num_imm += num_imm_operand;
  window_list->num_imm_32 += num_imm32_operand;
  window_list->num_imm_64 += num_imm64_operand;

  if (group == disp_store)
    window_list->num_stores += 1;
  else if (group == disp_load
	   || group == disp_prefetch)
    window_list->num_loads += 1;
  else if (group == disp_load_store)
    {
      window_list->num_stores += 1;
      window_list->num_loads += 1;
    }
}

/* Adds a scheduled instruction, INSN, to the current dispatch window.
   If the total bytes of instructions or the number of instructions in
   the window exceed allowable, it allocates a new window.  */

static void
add_to_dispatch_window (rtx_insn *insn)
{
  int byte_len;
  dispatch_windows *window_list;
  dispatch_windows *next_list;
  dispatch_windows *window0_list;
  enum insn_path path;
  enum dispatch_group insn_group;
  bool insn_fits;
  int num_insn;
  int num_uops;
  int window_num;
  int insn_num_uops;
  int sum;

  if (INSN_CODE (insn) < 0)
    return;

  byte_len = ix86_min_insn_size (insn);
  window_list = dispatch_window_list;
  next_list = window_list->next;
  path = get_insn_path (insn);
  insn_group = get_insn_group (insn);

  /* Get the last dispatch window.  */
  if (next_list)
      window_list = dispatch_window_list->next;

  if (path == path_single)
    insn_num_uops = 1;
  else if (path == path_double)
    insn_num_uops = 2;
  else
    insn_num_uops = (int) path;

  /* If current window is full, get a new window.
     Window number zero is full, if MAX_INSN uops are scheduled in it.
     Window number one is full, if window zero's bytes plus window
     one's bytes is 32, or if the bytes of the new instruction added
     to the total makes it greater than 48, or it has already MAX_INSN
     instructions in it.  */
  num_insn = window_list->num_insn;
  num_uops = window_list->num_uops;
  window_num = window_list->window_num;
  insn_fits = fits_dispatch_window (insn);

  if (num_insn >= MAX_INSN
      || num_uops + insn_num_uops > MAX_INSN
      || !(insn_fits))
    {
      window_num = ~window_num & 1;
      window_list = allocate_next_window (window_num);
    }

  if (window_num == 0)
    {
      add_insn_window (insn, window_list, insn_num_uops);
      if (window_list->num_insn >= MAX_INSN
	  && insn_group == disp_branch)
	{
	  process_end_window ();
	  return;
	}
    }
  else if (window_num == 1)
    {
      window0_list = window_list->prev;
      sum = window0_list->window_size + window_list->window_size;
      if (sum == 32
	  || (byte_len + sum) >= 48)
	{
	  process_end_window ();
	  window_list = dispatch_window_list;
	}

      add_insn_window (insn, window_list, insn_num_uops);
    }
  else
    gcc_unreachable ();

  if (is_end_basic_block (insn_group))
    {
      /* End of basic block is reached do end-basic-block process.  */
      process_end_window ();
      return;
    }
}

/* Print the dispatch window, WINDOW_NUM, to FILE.  */

DEBUG_FUNCTION static void
debug_dispatch_window_file (FILE *file, int window_num)
{
  dispatch_windows *list;
  int i;

  if (window_num == 0)
    list = dispatch_window_list;
  else
    list = dispatch_window_list1;

  fprintf (file, "Window #%d:\n", list->window_num);
  fprintf (file, "  num_insn = %d, num_uops = %d, window_size = %d\n",
	  list->num_insn, list->num_uops, list->window_size);
  fprintf (file, "  num_imm = %d, num_imm_32 = %d, num_imm_64 = %d, imm_size = %d\n",
	   list->num_imm, list->num_imm_32, list->num_imm_64, list->imm_size);

  fprintf (file, "  num_loads = %d, num_stores = %d\n", list->num_loads,
	  list->num_stores);
  fprintf (file, " insn info:\n");

  for (i = 0; i < MAX_INSN; i++)
    {
      if (!list->window[i].insn)
	break;
      fprintf (file, "    group[%d] = %s, insn[%d] = %p, path[%d] = %d byte_len[%d] = %d, imm_bytes[%d] = %d\n",
	      i, group_name[list->window[i].group],
	      i, (void *)list->window[i].insn,
	      i, list->window[i].path,
	      i, list->window[i].byte_len,
	      i, list->window[i].imm_bytes);
    }
}

/* Print to stdout a dispatch window.  */

DEBUG_FUNCTION void
debug_dispatch_window (int window_num)
{
  debug_dispatch_window_file (stdout, window_num);
}

/* Print INSN dispatch information to FILE.  */

DEBUG_FUNCTION static void
debug_insn_dispatch_info_file (FILE *file, rtx_insn *insn)
{
  int byte_len;
  enum insn_path path;
  enum dispatch_group group;
  int imm_size;
  int num_imm_operand;
  int num_imm32_operand;
  int num_imm64_operand;

  if (INSN_CODE (insn) < 0)
    return;

  byte_len = ix86_min_insn_size (insn);
  path = get_insn_path (insn);
  group = get_insn_group (insn);
  imm_size = get_num_immediates (insn, &num_imm_operand, &num_imm32_operand,
				 &num_imm64_operand);

  fprintf (file, " insn info:\n");
  fprintf (file, "  group = %s, path = %d, byte_len = %d\n",
	   group_name[group], path, byte_len);
  fprintf (file, "  num_imm = %d, num_imm_32 = %d, num_imm_64 = %d, imm_size = %d\n",
	   num_imm_operand, num_imm32_operand, num_imm64_operand, imm_size);
}

/* Print to STDERR the status of the ready list with respect to
   dispatch windows.  */

DEBUG_FUNCTION void
debug_ready_dispatch (void)
{
  int i;
  int no_ready = number_in_ready ();

  fprintf (stdout, "Number of ready: %d\n", no_ready);

  for (i = 0; i < no_ready; i++)
    debug_insn_dispatch_info_file (stdout, get_ready_element (i));
}

/* This routine is the driver of the dispatch scheduler.  */

void
ix86_bd_do_dispatch (rtx_insn *insn, int mode)
{
  if (mode == DISPATCH_INIT)
    init_dispatch_sched ();
  else if (mode == ADD_TO_DISPATCH_WINDOW)
    add_to_dispatch_window (insn);
}

/* Return TRUE if Dispatch Scheduling is supported.  */

bool
ix86_bd_has_dispatch (rtx_insn *insn, int action)
{
  /* Current implementation of dispatch scheduler models buldozer only.  */
  if ((TARGET_CPU_P (BDVER1) || TARGET_CPU_P (BDVER2)
       || TARGET_CPU_P (BDVER3) || TARGET_CPU_P (BDVER4))
      && flag_dispatch_scheduler)
    switch (action)
      {
      default:
	return false;

      case IS_DISPATCH_ON:
	return true;

      case IS_CMP:
	return is_cmp (insn);

      case DISPATCH_VIOLATION:
	return dispatch_violation ();

      case FITS_DISPATCH_WINDOW:
	return fits_dispatch_window (insn);
      }

  return false;
}
