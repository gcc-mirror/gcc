/* PR debug/54796 */
/* { dg-do run } */
/* { dg-options "-g" } */

__attribute__((noinline, noclone)) void
bar (char *a, int b)
{
  __asm volatile ("" : "+r" (a), "+r" (b) : : "memory");
}

__attribute__((noinline, noclone)) void
foo (int a, int b)
{
  int c = a;
  char d[b];	/* { dg-final { gdb-test 17 "a" "5" } } */
  bar (d, 2);	/* { dg-final { gdb-test 17 "b" "6" } } */
  bar (d, 4);	/* { dg-final { gdb-test 17 "c" "5" } } */
}

int
main ()
{
  foo (5, 6);
  return 0;
}
