/* Test various combinations of missing parentheses give the correct
   missing parenthesis message.  */

/* { dg-do preprocess } */

#if (1
#endif

#if 2 * (3 + 4
#endif

#if (2))
#endif

#if )
#endif

#if 4)
#endif

/* { dg-error "missing '\\)'" "missing ')' no. 1"  { target *-*-* } 6 } */
/* { dg-error "missing '\\)'" "missing ')' no. 2"  { target *-*-* } 9 } */
/* { dg-error "missing '\\('" "missing '(' no. 1"  { target *-*-* } 12 } */
/* { dg-error "missing '\\('" "missing '(' no. 2"  { target *-*-* } 15 } */
/* { dg-error "missing '\\('" "missing '(' no. 3"  { target *-*-* } 18 } */


