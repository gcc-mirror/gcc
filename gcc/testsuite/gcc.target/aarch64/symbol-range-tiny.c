/* { dg-do compile } */
/* { dg-options "-O3 -save-temps -mcmodel=tiny" } */

int fixed_regs[0x00200000];

int
foo()
{
  return fixed_regs[0x00080000];
}

/* { dg-final { scan-assembler-not "adr\tx\[0-9\]+, fixed_regs\\\+" } } */
