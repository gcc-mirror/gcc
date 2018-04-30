/* PR rtl-optimization/83364 */
/* { dg-do run } */
/* { dg-options "-O -fno-forward-propagate -fno-tree-coalesce-vars -fno-tree-ter" } */

int a;

static int __attribute__ ((noinline, noclone))
foo (unsigned char c)
{
  c <<= (long long) c != a;
  c = c << 7 | c >> 1;
  return c;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  if (foo (0) != 0)
    __builtin_abort ();
  return 0;
}
