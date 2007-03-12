/* Origin: PR 3467 */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
tdef (int n)
{
  typedef int A[n];	/* { dg-error "forbids variable length array" } */
  A a;
  A *p;
  p = &a;
}
