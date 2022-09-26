/* PR tree-optimization/106099 */
/* { dg-do compile } */
/* { dg-options "-O -fharden-compares -fno-tree-forwprop -fno-tree-ch -fno-tree-dominator-opts -fno-tree-ccp -funreachable-traps --param=scev-max-expr-size=1" } */

void
foo (void)
{
  for (unsigned i = 0; i == 0; i++)
    __builtin_printf ("%d", i);
}
