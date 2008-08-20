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
    x = p;
  else
    x = q;
  return *x;
}

