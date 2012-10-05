/* PR debug/54519 */
/* { dg-do run } */
/* { dg-options "-g" } */

__attribute__((noinline, noclone)) void
fn1 (int x)
{
  __asm volatile ("" : "+r" (x) : : "memory");
}

static int
fn2 (int x, int y)
{
  if (y)
    {
      fn1 (x);		/* { dg-final { gdb-test 17 "x" "6" } } */
      fn1 (x);		/* { dg-final { gdb-test 17 "y" "25" } } */
      fn1 (x);
      fn1 (x);
      y = -2 + x;
      y = y * y * y + y;
      fn1 (x + y);	/* { dg-final { gdb-test 22 "y" "68" } } */
    }
  return x;
}

int (*p) (int, int) = fn2;

int
main ()
{
  __asm volatile ("" : : : "memory");
  int (*q) (int, int) = p;
  __asm volatile ("" : : : "memory");
  q (6, 25);
  q (4, 117);
  q (0, 0);
  return 0;
}
