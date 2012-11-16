/* PR debug/54693 */
/* { dg-do run } */
/* { dg-options "-g" } */

int v;

__attribute__((noinline, noclone)) void
bar (int i)
{
  v = i;
  asm volatile ("" : : "r" (i) : "memory");
}

__attribute__((noinline, noclone)) void
foo (int x, int y, int z)
{
  int i = 0;
  while (x > 3 && y > 3 && z > 3)
    {		/* { dg-final { gdb-test 21 "i" "v + 1" } } */
		/* { dg-final { gdb-test 21 "x" "10 - i" } } */
      bar (i);	/* { dg-final { gdb-test 21 "y" "20 - 2 * i" } } */
		/* { dg-final { gdb-test 21 "z" "30 - 3 * i" } } */
      i++, x--, y -= 2, z -= 3;
    }
}

int
main ()
{
  v = -1;
  foo (10, 20, 30);
  return 0;
}
