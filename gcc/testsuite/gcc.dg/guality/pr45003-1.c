/* PR debug/45003 */
/* { dg-do run { target { x86_64-*-* && lp64 } } } */
/* { dg-options "-g" } */

int __attribute__((noinline))
foo (unsigned short *p)
{
  int a = *p;
  asm volatile ("nop");
  asm volatile ("nop" : : "D" (a));	/* { dg-final { gdb-test 10 "a" "0x8078" } } */
  return 0;
}

int __attribute__((noinline))
bar (short *p)
{
  unsigned int a = *p;
  asm volatile ("nop");
  asm volatile ("nop" : : "D" (a));	/* { dg-final { gdb-test 19 "a" "0xffff8078" } } */
  return 0;
}

int
main ()
{
  unsigned short us = 0x8078;
  foo (&us);
  short s = -32648;
  bar (&s);
  return 0;
}
