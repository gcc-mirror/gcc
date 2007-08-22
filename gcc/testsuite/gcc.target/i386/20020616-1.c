/* PR opt/6722 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#if !__PIC__
register int k asm("%ebx");
#elif __amd64
register int k asm("%r12");
#else
register int k asm("%esi");
#endif

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
