// { dg-do compile }

struct S {
  void *pp;
};

int main()
{
  int *sub1;

  /* No array section inside compound literal.  */
#pragma omp target update to( (struct S) { .pp = ([10][10]) sub1 } )
/* { dg-error {expected expression before '\[' token} "" { target *-*-* } .-1 } */
/* { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 } */

  return 0;
}
