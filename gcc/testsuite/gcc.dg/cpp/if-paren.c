/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* These now use "!=" rather than "<" to increase chance of failure.  */
#if 16 * (1) + 4 != 20
#error		/* { dg-bogus "error" "with paren" } */
#endif

#if 16 * 1 + 4 != 20
#error		/* { dg-bogus "error" "without paren" } */
#endif

#if ()	        /* { dg-error "expression between" "empty paren" } */
#endif

#if (1) == 1
#error		/* { dg-error "" "simple parens no. 1" } */
#endif

#if (2)
#error		/* { dg-error "" "simple parens no. 2" } */
#endif

#if 3 == (3)
#error		/* { dg-error "" "simple parens no. 3" } */
#endif

#if (((-1) + 8)) == ((+2) * ((3)) - -1)
#error		/* { dg-error "" "nested parentheses" } */
#endif
