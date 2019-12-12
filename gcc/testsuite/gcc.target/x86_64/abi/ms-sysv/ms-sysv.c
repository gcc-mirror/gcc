/* Test program for 64-Bit Microsoft to System V function calls.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Daniel Santos <daniel.santos@pobox.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is a single-threaded test program for Microsoft 64-bit ABI functions.
   It is aimed at verifying correctness of pro/epilogues of ms_abi functions
   that call sysv_abi functions to assure clobbered registers are properly
   saved and restored and attempt to detect any flaws in the behavior of these
   functions.  The following variants are tested:

   * Either uses hard frame pointer, re-aligns the stack or neither,
   * Uses alloca (and thus DRAP) or not,
   * Uses sibling call optimization or not,
   * Uses variable argument list or not, and
   * Has shrink-wrapped code or not.

  In addition, an ms_abi function is generated for each of these combinations
  clobbering each unique combination additional registers (excluding BP when
  a frame pointer is used). Shrink-wrap variants are called in a way that
  both the fast and slow path are used. Re-aligned variants are called with
  an aligned and mis-aligned stack.

  Each ms_abi function is called via an assembly stub that first saves all
  volatile registers and fills them with random values. The ms_abi function
  is then called.  After the function returns, the value of all volatile
  registers is verified against the random data and then restored.  */

/* { dg-do run } */
/* { dg-additional-sources "do-test.S" } */
/* { dg-additional-options "-Wall" } */
/* { dg-require-effective-target alloca } */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include <errno.h>
#include <ctype.h>

#if !defined(__x86_64__) || !defined(__SSE2__)
# error Test only valid on x86_64 with -msse2
#endif

enum reg_data_sets
{
  REG_SET_SAVE,
  REG_SET_INPUT,
  REG_SET_OUTPUT,

  REG_SET_COUNT
};

enum flags
{
  FLAG_ALLOCA			= 0x01000000,
  FLAG_SIBCALL			= 0x02000000,
  FLAG_SHRINK_WRAP_FAST_PATH	= 0x08000000,
  FLAG_SHRINK_WRAP_SLOW_PATH	= 0x0c000000,
};

enum alignment_option
{
  ALIGNMENT_NOT_TESTED,
  ALIGNMENT_ALIGNED,
  ALIGNMENT_MISALIGNED,

  ALIGNMENT_COUNT,
};

enum shrink_wrap_option
{
  SHRINK_WRAP_NONE,
  SHRINK_WRAP_FAST_PATH,
  SHRINK_WRAP_SLOW_PATH,

  SHRINK_WRAP_COUNT
};

union regdata {
  struct {
    __uint128_t	sseregs[10];
    union {
      uint64_t	intregs[8];
      struct {
	uint64_t	rsi;
	uint64_t	rdi;
	uint64_t	rbx;
	uint64_t	rbp;
	uint64_t	r12;
	uint64_t	r13;
	uint64_t	r14;
	uint64_t	r15;
      };
    };
  };
  uint32_t		u32_arr[56];
} __attribute__((aligned (16)));

struct test_data
{
  union regdata regdata[REG_SET_COUNT];
  void *fn;
  void *retaddr;
  const char *name;
  enum alignment_option alignment;
  enum shrink_wrap_option shrink_wrap;
  long ret_expected;
} test_data;

static int shrink_wrap_global;
static void __attribute((sysv_abi)) do_tests ();
static void init_test (void *fn, const char *name,
		       enum alignment_option alignment,
		       enum shrink_wrap_option shrink_wrap, long ret_expected);
static void check_results (long ret);
static __attribute__((ms_abi)) long do_sibcall (long arg);
static __attribute__((ms_abi)) long
(*const volatile do_sibcall_noinfo) (long) = do_sibcall;

/* Defines do_tests ().  */
#include "ms-sysv-generated.h"

static int arbitrarily_fail;
static const char *argv0;


#define PASTE_STR2(a)		#a
#define PASTE_STR1(a, b)	PASTE_STR2(a ## b)
#define PASTE_STR(a, b)		PASTE_STR1(a, b)

#ifdef __USER_LABEL_PREFIX__
# define ASMNAME(name)		PASTE_STR(__USER_LABEL_PREFIX__, name)
#else
# define ASMNAME(name)		#name
#endif

#ifdef __MACH__
# define LOAD_TEST_DATA_ADDR(dest) \
	"	mov	" ASMNAME(test_data) "@GOTPCREL(%%rip), " dest "\n"
#else
# define LOAD_TEST_DATA_ADDR(dest) \
	"	lea	" ASMNAME(test_data) "(%%rip), " dest "\n"
#endif

#define TEST_DATA_OFFSET(f)	((int)__builtin_offsetof(struct test_data, f))

void __attribute__((naked))
do_test_body (void)
{__asm__ (
	"	# rax, r10 and r11 are usable here.\n"
	"\n"
	"	# Save registers.\n"
		LOAD_TEST_DATA_ADDR("%%rax")
	"	lea	%p0(%%rax), %%r10\n"
	"	call	" ASMNAME(regs_to_mem) "\n"
	"\n"
	"	# Load registers with random data.\n"
	"	lea	%p1(%%rax), %%r10\n"
	"	call	" ASMNAME(mem_to_regs) "\n"
	"\n"
	"	# Pop and save original return address.\n"
	"	pop	%%r10\n"
	"	mov	%%r10, %p4(%%rax)\n"
	"\n"
	"	# Call the test function, after which rcx, rdx and r8-11\n"
	"	# become usable.\n"
	"	lea	%p3(%%rax), %%rax\n"
	"	call	*(%%rax)\n"
	"\n"
	"	# Store resulting register values.\n"
		LOAD_TEST_DATA_ADDR("%%rcx")
	"	lea	%p2(%%rcx), %%r10\n"
	"	call	" ASMNAME(regs_to_mem) "\n"
	"\n"
	"	# Push the original return address.\n"
	"	lea	%p4(%%rcx), %%r10\n"
	"	push	(%%r10)\n"
	"\n"
	"	# Restore registers.\n"
	"	lea	%p0(%%rcx), %%r10\n"
	"	call	" ASMNAME(mem_to_regs) "\n"
	"\n"
	"	retq\n"
	::
	"i"(TEST_DATA_OFFSET(regdata[REG_SET_SAVE])),
	"i"(TEST_DATA_OFFSET(regdata[REG_SET_INPUT])),
	"i"(TEST_DATA_OFFSET(regdata[REG_SET_OUTPUT])),
	"i"(TEST_DATA_OFFSET(fn)),
	"i"(TEST_DATA_OFFSET(retaddr)) : "memory");
}

static void __attribute__((noinline))
init_test (void *fn, const char *name, enum alignment_option alignment,
	   enum shrink_wrap_option shrink_wrap, long ret_expected)
{
  int i;
  union regdata *data = &test_data.regdata[REG_SET_INPUT];

  assert (alignment < ALIGNMENT_COUNT);
  assert (shrink_wrap < SHRINK_WRAP_COUNT);

  memset (&test_data, 0, sizeof (test_data));
  for (i = 55; i >= 0; --i)
    data->u32_arr[i] = (uint32_t)lrand48 ();
  test_data.fn = fn;
  test_data.name = name;
  test_data.alignment = alignment;
  test_data.shrink_wrap = shrink_wrap;
  test_data.ret_expected = ret_expected;

  switch (shrink_wrap)
  {
    case SHRINK_WRAP_NONE:
    case SHRINK_WRAP_COUNT:
      break;
    case SHRINK_WRAP_FAST_PATH:
      shrink_wrap_global = FLAG_SHRINK_WRAP_FAST_PATH;
      break;
    case SHRINK_WRAP_SLOW_PATH:
      shrink_wrap_global = FLAG_SHRINK_WRAP_SLOW_PATH;
      break;
  }
}

static const char *alignment_str[ALIGNMENT_COUNT] =
{
  "", "aligned", "misaligned"
};

static const char *shrink_wrap_str[SHRINK_WRAP_COUNT] =
{
  "", "shrink-wrap fast path", "shrink-wrap slow path"
};

static const char *test_descr ()
{
  static char buffer[0x400];

  if (test_data.alignment || test_data.shrink_wrap)
    snprintf (buffer, sizeof (buffer) - 1, "`%s' (%s%s%s)",
	      test_data.name,
	      alignment_str[test_data.alignment],
	      (test_data.alignment && test_data.shrink_wrap ? ", " : ""),
	      shrink_wrap_str[test_data.shrink_wrap]);
  else
    snprintf (buffer, sizeof (buffer) - 1, "`%s'", test_data.name);

  return buffer;
}

static const char *regnames[] = {
  "XMM6",
  "XMM7",
  "XMM8",
  "XMM9",
  "XMM10",
  "XMM11",
  "XMM12",
  "XMM13",
  "XMM14",
  "XMM15",
  "RSI",
  "RDI",
  "RBX",
  "RBP",
  "R12",
  "R13",
  "R14",
  "R15",
};

static void print_header (int *header_printed)
{
  if (!*header_printed)
    fprintf (stderr, "       %-35s    %-35s\n", "Expected", "Got");
  *header_printed = 1;
}

static int compare_reg128 (const __uint128_t *a, const __uint128_t *b,
			   const char *name, int *header_printed)
{
  if (!memcmp (a, b, sizeof (*a)))
    return 0;
  else
    {
      long ha = *((long*)a);
      long la = *((long*)a + 16);
      long hb = *((long*)b);
      long lb = *((long*)a + 16);
      print_header (header_printed);
      fprintf (stderr, "%-5s: 0x%016lx %016lx != 0x%016lx %016lx\n",
	       name, ha, la, hb, lb);
      return 1;
    }
}

static int compare_reg64 (long a, long b, const char *name,
			  int *header_printed)
{
  if (a == b)
    return 0;
  else
    {
      print_header (header_printed);
      fprintf (stderr, "%s: 0x%016lx != 0x%016lx\n", name, a, b);
      return 1;
    }
}


static void __attribute__((noinline)) check_results (long ret)
{
  unsigned i;
  unsigned bad = 0;
  int header_printed = 0;

  union regdata *a = &test_data.regdata[REG_SET_INPUT];
  union regdata *b = &test_data.regdata[REG_SET_OUTPUT];

  a = __builtin_assume_aligned(a, 16);
  b = __builtin_assume_aligned(b, 16);

  if (arbitrarily_fail) {
    uint64_t u64 = lrand48 ();
    if (u64 % 100 == 0)
      b->u32_arr[u64 % 56] = 0xfdfdfdfd;
  }

  for (i = 0; i < 10; ++i)
    bad |= compare_reg128 (&a->sseregs[i], &b->sseregs[i], regnames[i],
			   &header_printed);

  for (i = 0; i < 8; ++i)
    bad |= compare_reg64 (a->intregs[i], b->intregs[i], regnames[i + 10],
			  &header_printed);

  if (ret != test_data.ret_expected)
    {
      fprintf (stderr, "Wrong return value: got 0x%016lx, expected 0x%016lx\n",
	       ret, test_data.ret_expected);
      bad = 1;
    }

  if (bad)
    {
      fprintf (stderr, "Failed on test function %s\n", test_descr ());
      raise (SIGTRAP);
      exit (-1);
    }
}

static __attribute__((ms_abi, noinline)) long do_sibcall (long arg) {
  return arg + FLAG_SIBCALL;
}

void usage ()
{
  fprintf (stderr, "Usage: %s [-s <seed>] [-f]\n", argv0);
  exit (-1);
}

static long long_optarg (const char *optarg, const char *optstr)
{
  char *end;
  long ret;

  errno = 0;
  ret = strtol(optarg, &end, 0);

  while (isspace (*end))
    ++end;

  if (errno || *end)
    {
      fprintf (stderr, "ERROR: Bad value for %s: `%s`\n", optstr, optarg);
      if (errno)
	fprintf (stderr, "%s\n", strerror (errno));
      exit (-1);
    }

  return ret;
}

int main (int argc, char *argv[])
{
  long seed = 0;
  int c;
  argv0 = argv[0];

  assert (!((long)&test_data.regdata[REG_SET_SAVE] & 15));
  assert (!((long)&test_data.regdata[REG_SET_INPUT] & 15));
  assert (!((long)&test_data.regdata[REG_SET_OUTPUT] & 15));

  while ((c = getopt (argc, argv, "s:f")) != -1)
    {
      switch (c)
	{
	case 's':
	  seed = long_optarg (optarg, "-s");
	  break;

	case 'f':
	  arbitrarily_fail = 1;
	  fprintf (stderr, "NOTE: Aribrary failure enabled (-f).\n");
	  break;
	}
    }

  srand48 (seed);
  do_tests ();

  /* Just in case we don't have enough tests to randomly trigger the
     failure.  */
  if (arbitrarily_fail)
    return -1;

  return 0;
}
