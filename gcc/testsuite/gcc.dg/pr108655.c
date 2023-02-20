/* PR tree-optimization/108655 */
/* { dg-do compile } */
/* { dg-options "-w -O1 -funreachable-traps" } */

void
foo (void)
{
  int i, j;
  for (; i;)
    ;
  for (; i < 6;)
    for (j = 0; j < 6; ++j)
      i += j;
  __builtin_trap ();
}
