/* { dg-do compile } */
/* { dg-options "-march=armv8-a+sve -O2 -fharden-conditional-branches -fno-tree-scev-cprop" } */

/* -fharden-conditional-branches prevents optimization of its redundant
   compares by detaching values from the operands with asm statements.  They
   used to require GENERAL_REGS, but the vectorized booleans, generated while
   vectorizing this function, can't be held in GENERAL_REGS.  */

void
foo (int *p)
{
  while (*p < 1)
    ++*p;
}
