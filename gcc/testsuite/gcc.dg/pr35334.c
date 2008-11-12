/* PR c++/35334 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-bogus "not supported by" "" { target *-*-* } 0 } */

__complex__ unsigned int i;
int j;
char k;
__complex__ double l;
double m;
float n;

void
foo ()
{
  ((__complex__ int)i)();		/* { dg-error "is not a function" } */
  ((__complex__ int)j)();		/* { dg-error "is not a function" } */
  ((__complex__ int)k)();		/* { dg-error "is not a function" } */
  ((__complex__ long double)l)();	/* { dg-error "is not a function" } */
  ((__complex__ long double)m)();	/* { dg-error "is not a function" } */
  ((__complex__ long double)n)();	/* { dg-error "is not a function" } */
}
