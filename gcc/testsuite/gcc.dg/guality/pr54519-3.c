/* PR debug/54519 */
/* { dg-do run } */
/* { dg-options "-g" } */

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

int (*p) (int, int, int) = fn2;

int
main ()
{
  __asm volatile ("" : : : "memory");
  int (*q) (int, int, int) = p;
  __asm volatile ("" : : : "memory");
  q (36, 25, 6);
  q (98, 117, 8);
  q (0, 0, 0);
  return 0;
}
