/* { dg-do link } */
/* { dg-options "-O3 -save-temps -mcmodel=tiny" } */

char fixed_regs[0x00080000];

int
main ()
{
  return fixed_regs[0x000ff000];
}

/* { dg-final { scan-assembler-not "adr\tx\[0-9\]+, fixed_regs\\\+" } } */
