/* PR opt/6722 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-options "-O2" } */

register int k asm("%ebx");

void __attribute__((noinline))
foo()
{
  k = 1;
}

void test()
{
  int i;
  for (i = 0; i < 10; i += k)
    {
      k = 0;
      foo();
    }
}

int main()
{
  int old = k;
  test();
  k = old;
  return 0;
}
