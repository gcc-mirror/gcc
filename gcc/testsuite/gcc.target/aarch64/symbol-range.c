/* { dg-do compile } */
/* { dg-options "-O3 -save-temps -mcmodel=small" } */

int fixed_regs[0x200000000ULL];

int
foo()
{
  return fixed_regs[0x100000000ULL];
}

/* { dg-final { scan-assembler-not "adrp\tx\[0-9\]+, fixed_regs\\\+" } } */
