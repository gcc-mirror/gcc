/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

typedef _Complex float C;
C foo()
{
  C f;
  __imag__ f = 0;
  return f;	/* { dg-warning "is used" "unconditional" } */
}
