/* Test 'q' and 'Q' suffixes on __float128 type constants.  */
/* { dg-do compile { target { ia64-*-* || { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-options "" } */

__float128 a = 123.456789q;
__float128 b = 123.456789Q;
