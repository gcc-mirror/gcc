/* PR target/85593 */
/* { dg-do run { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-O2" } */

__attribute__((naked)) void
bar (void)
{
  asm ("xorl %eax, %eax\n\t"
       "xorl %edx, %edx\n\t"
       "xorl %ecx, %ecx\n\t"
       "xorl %esi, %esi\n\t"
       "xorl %edi, %edi\n\t"
       "xorl %r8d, %r8d\n\t"
       "xorl %r9d, %r9d\n\t"
       "xorl %r10d, %r10d\n\t"
       "xorl %r11d, %r11d\n\t"
       "ret");
}

int
main ()
{
  int a = 42;
  asm ("" : "+r" (a));
  bar ();
  asm ("" : "+r" (a));
  if (a != 42)
    __builtin_abort ();
  return 0;
}
