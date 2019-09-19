/* Test spelling differences in UCNs in macro definitions still count
   as the same identifier for macro expansion.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#define m1\u00c1
#ifndef m1Á
#error not defined
#endif

#define m2(\u00c1) Á

int i = m2 (0);
