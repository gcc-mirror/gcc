/* Test UCNs are allowed in preprocessing numbers.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#define a(x) b(x)
#define b(x) 0
#define p )
int c = a(0\u00c0.p);
