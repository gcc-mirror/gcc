/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test that all operators correctly short circuit.  */

#if (2 || 3 / 0) != 1
#error		/* { dg-bogus "error" "|| short circuit" } */
#endif

#if 0 && 3 / 0
#error		/* { dg-bogus "error" "&& short circuit" } */
#endif

#if 1 ? 0 : 3 / 0
#error		/* { dg-bogus "error" "? : right short circuit" } */
#endif

#if 0 ? 3 / 0 : 2
#else
#error		/* { dg-bogus "error" "? : left short circuit" } */
#endif

#if -1 ? 0 && 3 / 0 : 3 / 0 + 5 == 5
#error		/* { dg-bogus "error" "nested short circuiting" } */
#endif
