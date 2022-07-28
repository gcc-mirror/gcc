/* PR tree-optimization/106099 */
/* { dg-do compile } */
/* { dg-options "-O -fsanitize=unreachable -fsanitize-undefined-trap-on-error -fno-tree-ccp -fno-tree-dominator-opts" } */

void
foo (void)
{
  for (unsigned i = 0; i == 0; i++)
    ;
}
