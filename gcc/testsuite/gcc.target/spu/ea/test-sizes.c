/* Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* { dg-do run } */

#ifdef __EA32__
#define EA_PTRSIZE 4
#endif
#ifdef __EA64__
#define EA_PTRSIZE 8
#endif

#if !defined(LEVEL1) && !defined(LEVEL2) && !defined(LEVEL3)
#define LEVEL1 1		/* single pointer indirection */
#define LEVEL2 1		/* 2 levels of pointer indirection */
#define LEVEL3 1		/* 3 levels of pointer indirection */

#else
#ifndef LEVEL1
#define LEVEL1 0
#endif

#ifndef LEVEL2
#define LEVEL2 0
#endif

#ifndef LEVEL3
#define LEVEL3 0
#endif
#endif

#if !defined(USE_SIMPLE) && !defined(USE_COMPLEX)
#define USE_SIMPLE  1		/* build up pointers via multiple typedefs */
#define USE_COMPLEX 1		/* single typedef for pointer indirections */

#else
#ifndef USE_SIMPLE
#define USE_SIMPLE 0
#endif

#ifndef USE_COMPLEX
#define USE_COMPLEX 0
#endif
#endif

#if !defined(USE_LOCAL_VAR) && !defined(USE_EA_VAR)
#define USE_LOCAL_VAR 1		/* use variables declared locally */
#define USE_EA_VAR    1		/* use variables on the host */

#else
#ifndef USE_LOCAL_VAR
#define USE_LOCAL_VAR 0
#endif

#ifndef USE_EA_VAR
#define USE_EA_VAR    0
#endif
#endif

static int errors;

#ifdef USE_PRINTF		/* print results via printf */
#include <stdio.h>
#include <stdlib.h>

static int num_tests;

#define TEST_SIZE(EXPR, EXPECTED)					\
do {									\
  char *msg;								\
									\
  if (sizeof (EXPR) != EXPECTED)					\
    {									\
      msg = ", FAIL";							\
      errors++;								\
    }									\
  else									\
    msg = "";								\
									\
  num_tests++;								\
  printf ("sizeof %-20s = %2u, expected = %2u%s\n",			\
	  #EXPR,							\
	  (unsigned) sizeof (EXPR),					\
	  (unsigned) EXPECTED,						\
	  msg);								\
} while (0)

#define PRINT1(FMT)	  printf (FMT)
#define PRINT2(FMT,A1)	  printf (FMT,A1)
#define PRINT3(FMT,A1,A2) printf (FMT,A1,A2)

#else	/* standalone */
extern void abort (void);

#define TEST_SIZE(EXPR, EXPECTED)					\
do {									\
  if (sizeof (EXPR) != EXPECTED)					\
    abort ();								\
} while (0)

#define PRINT1(FMT)
#define PRINT2(FMT,ARG)
#define PRINT3(FMT,A1,A2)
#endif

/* 'local memory' hack to keep the same spacing.  */
#define __lm

#if USE_SIMPLE
#if (LEVEL1 || LEVEL2 || LEVEL3)
typedef __lm char *lm_ptr_t;
typedef __ea char *ea_ptr_t;
#endif

#if LEVEL1
#if USE_LOCAL_VAR
__lm lm_ptr_t lm_ptr;
__lm ea_ptr_t ea_ptr;
#endif

#if USE_EA_VAR
__ea lm_ptr_t lm_ptr_ea;
__ea ea_ptr_t ea_ptr_ea;
#endif
#endif

#if (LEVEL2 || LEVEL3)
typedef __lm lm_ptr_t *lm_lm_ptr_t;
typedef __ea lm_ptr_t *ea_lm_ptr_t;
typedef __lm ea_ptr_t *lm_ea_ptr_t;
typedef __ea ea_ptr_t *ea_ea_ptr_t;
#endif

#if LEVEL2
#if USE_LOCAL_VAR
__lm lm_lm_ptr_t lm_lm_ptr;
__lm ea_lm_ptr_t ea_lm_ptr;
__lm lm_ea_ptr_t lm_ea_ptr;
__lm ea_ea_ptr_t ea_ea_ptr;
#endif

#if USE_EA_VAR
__ea lm_lm_ptr_t lm_lm_ptr_ea;
__ea ea_lm_ptr_t ea_lm_ptr_ea;
__ea lm_ea_ptr_t lm_ea_ptr_ea;
__ea ea_ea_ptr_t ea_ea_ptr_ea;
#endif
#endif

#if LEVEL3
typedef __lm lm_lm_ptr_t *lm_lm_lm_ptr_t;
typedef __ea lm_lm_ptr_t *ea_lm_lm_ptr_t;
typedef __lm ea_lm_ptr_t *lm_ea_lm_ptr_t;
typedef __ea ea_lm_ptr_t *ea_ea_lm_ptr_t;
typedef __lm lm_ea_ptr_t *lm_lm_ea_ptr_t;
typedef __ea lm_ea_ptr_t *ea_lm_ea_ptr_t;
typedef __lm ea_ea_ptr_t *lm_ea_ea_ptr_t;
typedef __ea ea_ea_ptr_t *ea_ea_ea_ptr_t;

#if USE_LOCAL_VAR
__lm lm_lm_lm_ptr_t lm_lm_lm_ptr;
__lm ea_lm_lm_ptr_t ea_lm_lm_ptr;
__lm lm_ea_lm_ptr_t lm_ea_lm_ptr;
__lm ea_ea_lm_ptr_t ea_ea_lm_ptr;
__lm lm_lm_ea_ptr_t lm_lm_ea_ptr;
__lm ea_lm_ea_ptr_t ea_lm_ea_ptr;
__lm lm_ea_ea_ptr_t lm_ea_ea_ptr;
__lm ea_ea_ea_ptr_t ea_ea_ea_ptr;
#endif

#if USE_EA_VAR
__ea lm_lm_lm_ptr_t lm_lm_lm_ptr_ea;
__ea ea_lm_lm_ptr_t ea_lm_lm_ptr_ea;
__ea lm_ea_lm_ptr_t lm_ea_lm_ptr_ea;
__ea ea_ea_lm_ptr_t ea_ea_lm_ptr_ea;
__ea lm_lm_ea_ptr_t lm_lm_ea_ptr_ea;
__ea ea_lm_ea_ptr_t ea_lm_ea_ptr_ea;
__ea lm_ea_ea_ptr_t lm_ea_ea_ptr_ea;
__ea ea_ea_ea_ptr_t ea_ea_ea_ptr_ea;
#endif
#endif
#endif

#if USE_COMPLEX
#if LEVEL1
#if USE_LOCAL_VAR
__lm char *__lm lm_cptr;
__ea char *__lm ea_cptr;
#endif

#if USE_EA_VAR
__lm char *__ea lm_cptr_ea;
__ea char *__ea ea_cptr_ea;
#endif
#endif

#if LEVEL2
#if USE_LOCAL_VAR
__lm char *__lm *__lm lm_lm_cptr;
__lm char *__ea *__lm ea_lm_cptr;
__ea char *__lm *__lm lm_ea_cptr;
__ea char *__ea *__lm ea_ea_cptr;
#endif

#if USE_EA_VAR
__lm char *__lm *__ea lm_lm_cptr_ea;
__lm char *__ea *__ea ea_lm_cptr_ea;
__ea char *__lm *__ea lm_ea_cptr_ea;
__ea char *__ea *__ea ea_ea_cptr_ea;
#endif
#endif

#if LEVEL3
#if USE_LOCAL_VAR
__lm char *__lm *__lm *__lm lm_lm_lm_cptr;
__lm char *__ea *__lm *__lm lm_ea_lm_cptr;
__ea char *__lm *__lm *__lm lm_lm_ea_cptr;
__ea char *__ea *__lm *__lm lm_ea_ea_cptr;
__lm char *__lm *__ea *__lm ea_lm_lm_cptr;
__lm char *__ea *__ea *__lm ea_ea_lm_cptr;
__ea char *__lm *__ea *__lm ea_lm_ea_cptr;
__ea char *__ea *__ea *__lm ea_ea_ea_cptr;
#endif

#if USE_EA_VAR
__lm char *__lm *__lm *__ea lm_lm_lm_cptr_ea;
__lm char *__ea *__lm *__ea lm_ea_lm_cptr_ea;
__ea char *__lm *__lm *__ea lm_lm_ea_cptr_ea;
__ea char *__ea *__lm *__ea lm_ea_ea_cptr_ea;
__lm char *__lm *__ea *__ea ea_lm_lm_cptr_ea;
__lm char *__ea *__ea *__ea ea_ea_lm_cptr_ea;
__ea char *__lm *__ea *__ea ea_lm_ea_cptr_ea;
__ea char *__ea *__ea *__ea ea_ea_ea_cptr_ea;
#endif
#endif
#endif

int
main ()
{
  PRINT2 ("LEVEL1        = %d\n", LEVEL1);
  PRINT2 ("LEVEL2        = %d\n", LEVEL2);
  PRINT2 ("LEVEL3        = %d\n", LEVEL3);
  PRINT2 ("USE_SIMPLE    = %d\n", USE_SIMPLE);
  PRINT2 ("USE_COMPLEX   = %d\n", USE_COMPLEX);
  PRINT2 ("USE_LOCAL_VAR = %d\n", USE_LOCAL_VAR);
  PRINT2 ("USE_EA_VAR    = %d\n", USE_EA_VAR);
  PRINT1 ("\n");

#if USE_SIMPLE
#if LEVEL1
#if USE_LOCAL_VAR
  TEST_SIZE ( lm_ptr, 4);
  TEST_SIZE (*lm_ptr, 1);
  TEST_SIZE ( ea_ptr, EA_PTRSIZE);
  TEST_SIZE (*ea_ptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE ( lm_ptr_ea, 4);
  TEST_SIZE (*lm_ptr_ea, 1);
  TEST_SIZE ( ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (*ea_ptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif

#if LEVEL2
#if USE_LOCAL_VAR
  TEST_SIZE (  lm_lm_ptr, 4);
  TEST_SIZE ( *lm_lm_ptr, 4);
  TEST_SIZE (**lm_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  lm_ea_ptr, 4);
  TEST_SIZE ( *lm_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (**lm_ea_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_lm_ptr, EA_PTRSIZE);
  TEST_SIZE ( *ea_lm_ptr, 4);
  TEST_SIZE (**ea_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE ( *ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (**ea_ea_ptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE (  lm_lm_ptr_ea, 4);
  TEST_SIZE ( *lm_lm_ptr_ea, 4);
  TEST_SIZE (**lm_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  lm_ea_ptr_ea, 4);
  TEST_SIZE ( *lm_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (**lm_ea_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_lm_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( *ea_lm_ptr_ea, 4);
  TEST_SIZE (**ea_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( *ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (**ea_ea_ptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif

#if LEVEL3
#if USE_LOCAL_VAR
  TEST_SIZE (   lm_lm_lm_ptr, 4);
  TEST_SIZE (  *lm_lm_lm_ptr, 4);
  TEST_SIZE ( **lm_lm_lm_ptr, 4);
  TEST_SIZE (***lm_lm_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_lm_ea_ptr, 4);
  TEST_SIZE (  *lm_lm_ea_ptr, 4);
  TEST_SIZE ( **lm_lm_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (***lm_lm_ea_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_lm_ptr, 4);
  TEST_SIZE (  *lm_ea_lm_ptr, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_lm_ptr, 4);
  TEST_SIZE (***lm_ea_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_ea_ptr, 4);
  TEST_SIZE (  *lm_ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (***lm_ea_ea_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_lm_ptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_lm_ptr, 4);
  TEST_SIZE ( **ea_lm_lm_ptr, 4);
  TEST_SIZE (***ea_lm_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_ea_ptr, 4);
  TEST_SIZE ( **ea_lm_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (***ea_lm_ea_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_lm_ptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_lm_ptr, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_lm_ptr, 4);
  TEST_SIZE (***ea_ea_lm_ptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_ea_ptr, EA_PTRSIZE);
  TEST_SIZE (***ea_ea_ea_ptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE (   lm_lm_lm_ptr_ea, 4);
  TEST_SIZE (  *lm_lm_lm_ptr_ea, 4);
  TEST_SIZE ( **lm_lm_lm_ptr_ea, 4);
  TEST_SIZE (***lm_lm_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_lm_ea_ptr_ea, 4);
  TEST_SIZE (  *lm_lm_ea_ptr_ea, 4);
  TEST_SIZE ( **lm_lm_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (***lm_lm_ea_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_lm_ptr_ea, 4);
  TEST_SIZE (  *lm_ea_lm_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_lm_ptr_ea, 4);
  TEST_SIZE (***lm_ea_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_ea_ptr_ea, 4);
  TEST_SIZE (  *lm_ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (***lm_ea_ea_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_lm_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_lm_ptr_ea, 4);
  TEST_SIZE ( **ea_lm_lm_ptr_ea, 4);
  TEST_SIZE (***ea_lm_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_ea_ptr_ea, 4);
  TEST_SIZE ( **ea_lm_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (***ea_lm_ea_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_lm_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_lm_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_lm_ptr_ea, 4);
  TEST_SIZE (***ea_ea_lm_ptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_ea_ptr_ea, EA_PTRSIZE);
  TEST_SIZE (***ea_ea_ea_ptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif
#endif

#if USE_COMPLEX
#if LEVEL1
#if USE_LOCAL_VAR
  TEST_SIZE ( lm_cptr, 4);
  TEST_SIZE (*lm_cptr, 1);
  TEST_SIZE ( ea_cptr, EA_PTRSIZE);
  TEST_SIZE (*ea_cptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE ( lm_cptr_ea, 4);
  TEST_SIZE (*lm_cptr_ea, 1);
  TEST_SIZE ( ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (*ea_cptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif

#if LEVEL2
#if USE_LOCAL_VAR
  TEST_SIZE (  lm_lm_cptr, 4);
  TEST_SIZE ( *lm_lm_cptr, 4);
  TEST_SIZE (**lm_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  lm_ea_cptr, 4);
  TEST_SIZE ( *lm_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (**lm_ea_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_lm_cptr, EA_PTRSIZE);
  TEST_SIZE ( *ea_lm_cptr, 4);
  TEST_SIZE (**ea_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE ( *ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (**ea_ea_cptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE (  lm_lm_cptr_ea, 4);
  TEST_SIZE ( *lm_lm_cptr_ea, 4);
  TEST_SIZE (**lm_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  lm_ea_cptr_ea, 4);
  TEST_SIZE ( *lm_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (**lm_ea_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_lm_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( *ea_lm_cptr_ea, 4);
  TEST_SIZE (**ea_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (  ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( *ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (**ea_ea_cptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif

#if LEVEL3
#if USE_LOCAL_VAR
  TEST_SIZE (   lm_lm_lm_cptr, 4);
  TEST_SIZE (  *lm_lm_lm_cptr, 4);
  TEST_SIZE ( **lm_lm_lm_cptr, 4);
  TEST_SIZE (***lm_lm_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_lm_ea_cptr, 4);
  TEST_SIZE (  *lm_lm_ea_cptr, 4);
  TEST_SIZE ( **lm_lm_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (***lm_lm_ea_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_lm_cptr, 4);
  TEST_SIZE (  *lm_ea_lm_cptr, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_lm_cptr, 4);
  TEST_SIZE (***lm_ea_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_ea_cptr, 4);
  TEST_SIZE (  *lm_ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (***lm_ea_ea_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_lm_cptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_lm_cptr, 4);
  TEST_SIZE ( **ea_lm_lm_cptr, 4);
  TEST_SIZE (***ea_lm_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_ea_cptr, 4);
  TEST_SIZE ( **ea_lm_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (***ea_lm_ea_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_lm_cptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_lm_cptr, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_lm_cptr, 4);
  TEST_SIZE (***ea_ea_lm_cptr, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_ea_cptr, EA_PTRSIZE);
  TEST_SIZE (***ea_ea_ea_cptr, 1);
  PRINT1 ("\n");
#endif

#if USE_EA_VAR
  TEST_SIZE (   lm_lm_lm_cptr_ea, 4);
  TEST_SIZE (  *lm_lm_lm_cptr_ea, 4);
  TEST_SIZE ( **lm_lm_lm_cptr_ea, 4);
  TEST_SIZE (***lm_lm_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_lm_ea_cptr_ea, 4);
  TEST_SIZE (  *lm_lm_ea_cptr_ea, 4);
  TEST_SIZE ( **lm_lm_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (***lm_lm_ea_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_lm_cptr_ea, 4);
  TEST_SIZE (  *lm_ea_lm_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_lm_cptr_ea, 4);
  TEST_SIZE (***lm_ea_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   lm_ea_ea_cptr_ea, 4);
  TEST_SIZE (  *lm_ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **lm_ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (***lm_ea_ea_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_lm_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_lm_cptr_ea, 4);
  TEST_SIZE ( **ea_lm_lm_cptr_ea, 4);
  TEST_SIZE (***ea_lm_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_lm_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_lm_ea_cptr_ea, 4);
  TEST_SIZE ( **ea_lm_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (***ea_lm_ea_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_lm_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_lm_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_lm_cptr_ea, 4);
  TEST_SIZE (***ea_ea_lm_cptr_ea, 1);
  PRINT1 ("\n");

  TEST_SIZE (   ea_ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (  *ea_ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE ( **ea_ea_ea_cptr_ea, EA_PTRSIZE);
  TEST_SIZE (***ea_ea_ea_cptr_ea, 1);
  PRINT1 ("\n");
#endif
#endif
#endif

  if (errors)
    {
      PRINT3 ("%d error(s), %d test(s)\n", errors, num_tests);
      abort ();
    }
  else
    PRINT2 ("No errors, %d test(s)\n", num_tests);

  return 0;
}
