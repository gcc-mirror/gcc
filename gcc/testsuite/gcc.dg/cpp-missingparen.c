/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* Test various combinations of missing parentheses give the correct
   missing parenthesis message.  */

/* { dg-do preprocess } */

#if (1          /* { dg-error "missing '\\)'" "missing ')' no. 1" } */
#endif

#if 2 * (3 + 4	/* { dg-error "missing '\\)'" "missing ')' no. 2" } */
#endif

#if (2))	/* { dg-error "missing '\\('" "missing '(' no. 1" } */
#endif

#if )		/* { dg-error "missing '\\('" "missing '(' no. 2" } */
#endif

#if 4)		/* { dg-error "missing '\\('" "missing '(' no. 3" } */
#endif
