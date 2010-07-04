/* PR debug/43593 */
/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
bar (int *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

int __attribute__((noinline))
foo (void)
{
  int i, *j = &i;
  bar (j);
  return 6 + i;	/* { dg-final { gdb-test 16 "j" "&i" } } */
}

int
main (void)
{
  foo ();
  return 0;
}
