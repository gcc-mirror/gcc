/* { dg-do compile } */

int *a, *b, *c;

void foo()
{
  int i, j;
  // The initialization shall declare or initialize a *SINGLE* variable.
#pragma simd
  for (i=0, j=5; i < 1000; i++) // { dg-error "expected ';' before ','" }
    a[i] = b[j];
}
