/* Origin: PR 3467 */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
tdef (int n)
{
  typedef int A[n];	/* { dg-bogus "forbids variable-size array" } */
  A a;
  A *p;
  p = &a;
}
