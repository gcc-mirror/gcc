/* { dg-do compile { target ia32 } } */
/* { dg-options "-mpreferred-stack-boundary=2 -Os -w" { target { i?86-*-* x86_64-*-* } } } */

int a;

long long __attribute__((noinline))
b (void)
{
}

void
c (void)
{
  if (b())
    a = 1;
}
