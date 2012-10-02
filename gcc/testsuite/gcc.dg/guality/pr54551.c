/* PR debug/54551 */
/* { dg-do run } */
/* { dg-options "-g" } */

void  __attribute__((__noinline__))
bar (void)
{
  asm volatile ("");
}

int __attribute__((__noinline__))
foo (int x, int y, int z)
{
  if (x != z)
    {
      int a = z + 1;
      bar (); /* { dg-final { gdb-test 18 "a" "4" } } */
      bar (); /* { dg-final { gdb-test 18 "z" "3" } } */
    }
  return y;
}

int
main ()
{
  foo (1, 2, 3);
  return 0;
}
