/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized -ftrivial-auto-var-init=zero" } */

typedef _Complex float C;
C foo(int cond)
{
  C f;
  __imag__ f = 0;
  if (cond)
    {
      __real__ f = 1;
      return f;
    }
  return f;	/* { dg-warning "may be used" "unconditional" } */
}
