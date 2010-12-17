/* PR debug/45003 */
/* { dg-do run { target { x86_64-*-* && lp64 } } } */
/* { dg-options "-g" } */

int __attribute__((noinline))
foo (unsigned short *p)
{
  int a = (short) *p;
  asm volatile ("nop" : : "D" ((int) *p));
  asm volatile ("nop" : : "D" ((int) *p));	/* { dg-final { gdb-test 10 "a" "-32648" } } */
  return 0;
}

int __attribute__((noinline))
bar (short *p)
{
  unsigned int a = (unsigned short) *p;
  asm volatile ("nop" : : "D" ((unsigned int) *p));
  asm volatile ("nop" : : "D" ((unsigned int) *p));	/* { dg-final { gdb-test 19 "a" "0x8078" } } */
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
