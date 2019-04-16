/* PR tree-optimization/89487 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

#include "../pr87600.h"

void
caml_interprete (void)
{
#if defined(REG1) && defined(REG2)
  register int *pc asm(REG1);
  register int *sp asm(REG2);
  int i;

  for (i = 0; i < 3; ++i)
    *--sp = pc[i];
#endif
}
