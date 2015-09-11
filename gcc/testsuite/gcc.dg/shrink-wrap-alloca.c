/* { dg-do compile } */
/* { dg-options "-O2 -g" } */
/* { dg-require-effective-target alloca } */

int *p;

void
test (int a)
{
  if (a > 0)
    p = __builtin_alloca (4);
}
