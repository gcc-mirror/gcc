/* PR middle-end/113921 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) long
foo (void)
{
  long v;
  asm volatile goto ("jmp %l2" : "=r" (v) : "0" (27) : : lab);
  return v;
lab:
  return 42;
}

int
main ()
{
  if (foo () != 42)
    __builtin_abort ();
}
