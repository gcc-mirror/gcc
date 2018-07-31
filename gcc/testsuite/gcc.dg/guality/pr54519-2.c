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
      fn1 (x);		/* { dg-final { gdb-test .+1 "x" "6" } } */
      fn1 (x);		/* { dg-final { gdb-test . "y" "25" } } */
      fn1 (x);
      fn1 (x);
      y = -2 + x;
      y = y * y * y + y;
      fn1 (x + y);	/* { dg-final { gdb-test . "y" "68" } } */
    }
  return x;
}

__attribute__((noinline, noclone)) int
fn3 (int x, int y)
{
  return fn2 (x, y) + y;
}

__attribute__((noinline, noclone)) int
fn4 (int x, int y)
{
  return fn2 (x, y) + y;
}

int
main ()
{
  fn3 (6, 25);
  fn4 (4, 117);
  return 0;
}
