/* PR debug/43479 */
/* { dg-do run } */
/* { dg-options "-g" } */

__attribute__((noinline)) void
foo (int k, int l, int m, int n)
{
  l++;
  {
    int h = n;
    {
      int i = k;
      k++;	/* { dg-final { gdb-test 13 "i" "6" } } */
    }		/* { dg-final { gdb-test 13 "h" "9" } } */
		/* { dg-final { gdb-test 13 "n" "9" } } */
    {
      int j = m;
      m++;	/* { dg-final { gdb-test 18 "j" "8" } } */
    }		/* { dg-final { gdb-test 18 "h" "9" } } */
		/* { dg-final { gdb-test 12 "n" "9" } } */
  }
  asm volatile ("" : : "r" (k), "r" (l));
  asm volatile ("" : : "r" (m), "r" (n));
}

int
main (void)
{
  int q = 6;
  asm ("" : "+r" (q));
  foo (q, q + 1, q + 2, q + 3);
  return 0;
}
