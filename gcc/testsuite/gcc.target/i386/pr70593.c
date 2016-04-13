/* PR middle-end/70593 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noinline, noclone)) unsigned long
foo (unsigned x)
{
  unsigned long a, c = x;
  asm volatile ("xorl\t%k1, %k1\n\tmovl\t$7, %k0" : "=c" (c), "=a" (a) : "0" (c), "1" (c) : "memory");
  return c;
}

int
main ()
{
  if (foo (3) != 7)
    __builtin_abort ();
  return 0;
}
