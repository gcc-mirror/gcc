/* PR tree-optimization/46491 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

__attribute__((noinline)) int
foo (int *p)
{
  int r;
  asm ("movl $6, (%1)\n\txorl %0, %0" : "=r" (r) : "r" (p) : "memory");
  return r;
}

int
main (void)
{
  int p = 8;
  if ((foo (&p) ? : p) != 6)
    abort ();
  return 0;
}
