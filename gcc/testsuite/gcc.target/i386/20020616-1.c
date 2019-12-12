/* PR opt/6722 */
/* { dg-do run } */
/* { dg-options "-O2" } */

/* We need this type to be as wide as the register chosen below, so
   that, when we preserve it across main, we preserve all of it.  */
typedef int __attribute__ ((mode (__word__))) reg_type;

#if !__PIC__
register reg_type k asm("%ebx");
#elif __amd64
register reg_type k asm("%r12");
#else
register reg_type k asm("%esi");
#endif

void __attribute__((noinline))
foo()
{
  k = 1;
}

void test()
{
  reg_type i;
  for (i = 0; i < 10; i += k)
    {
      k = 0;
      foo();
    }
}

int main()
{
  reg_type old = k;
  test();
  k = old;
  return 0;
}
