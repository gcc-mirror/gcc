/* { dg-do compile { target ia32 } } */
/* { dg-options "-mpreferred-stack-boundary=2 -Os -w" } */

int a;

long long
b (void)
{
}

void
c (void)
{
  if (b())
    a = 1;
}
