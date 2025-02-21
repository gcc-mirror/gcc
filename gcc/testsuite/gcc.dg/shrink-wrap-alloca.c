/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int *p;

void
test (int a)
{
  if (a > 0)
    p = __builtin_alloca (4);
}
