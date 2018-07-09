/* PR debug/54519 */
/* { dg-do run } */
/* { dg-options "-g -fno-ipa-vrp" } */

__attribute__((noinline, noclone)) void
fn1 (int x)
{
  __asm volatile ("" : "+r" (x) : : "memory");
}

static int
fn2 (int x, int y, int z)
{
  int a = 8;
  if (x != z)
    {
      fn1 (x);
      fn1 (x);		/* { dg-final { gdb-test .+2 "x" "36" } } */
      if (x == 36)	/* { dg-final { gdb-test .+1 "y" "25" } } */
	fn1 (x);	/* { dg-final { gdb-test . "z" "6" } } */
      fn1 (x);		/* { dg-final { gdb-test .+2 "x" "98" } } */
      if (x == 98)	/* { dg-final { gdb-test .+1 "y" "117" } } */
	fn1 (x);	/* { dg-final { gdb-test . "z" "8" } } */
      fn1 (x);
      fn1 (x + a);
    }
  return y;
}

__attribute__((noinline, noclone)) int
fn3 (int x, int y)
{
  return fn2 (x, y, 6);
}

__attribute__((noinline, noclone)) int
fn4 (int x, int y)
{
  return fn2 (x, y, 8);
}

int
main ()
{
  fn3 (36, 25);
  fn4 (98, 117);
  return 0;
}
