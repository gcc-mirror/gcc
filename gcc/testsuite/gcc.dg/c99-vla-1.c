/* Origin: PR 3467 */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */
/* { dg-require-effective-target alloca } */

void
tdef (int n)
{
  typedef int A[n];	/* { dg-bogus "forbids variable length array" } */
  A a;
  A *p;
  p = &a;
}
