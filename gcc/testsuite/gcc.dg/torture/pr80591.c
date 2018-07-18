/* PR tree-optimization/80591 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } "-flto" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

static inline __attribute__((always_inline)) int *
foo (void)
{
  __UINTPTR_TYPE__ sp;
  asm ("" : "=r" (sp));
  return (int *) sp;
}

void
bar (void)
{
  foo ()[0] += 26;
}

/* { dg-final { scan-tree-dump "\\+ 26;" "optimized" } } */
