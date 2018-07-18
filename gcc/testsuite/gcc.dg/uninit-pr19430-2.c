/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int  *p, *q;

int foo (int b)
{
  int i, j = 0;
  int *x;
  p = &i;
  q = &j;
  if (b)
    x = p;  /* { dg-warning "i. may be used uninitialized" } */
  else
    x = q;
  return *x;
}

