/* PR tree-optimization/109362 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -masm=att" } */
/* Ensure we don't waste a register set to %rdi + 8.  */
/* { dg-final { scan-assembler "\tmovq\t\\\(%rdi\\\), %r" } } */
/* { dg-final { scan-assembler "\tmovq\t8\\\(%rdi\\\), %r" } } */

struct S { long a, b; };

int
foo (struct S *v)
{
  while (1)
    {
      __atomic_load_n (&v->a, __ATOMIC_ACQUIRE);
      if (__atomic_load_n (&v->b, __ATOMIC_ACQUIRE))
	return 1;
    }
}
