/* { dg-do link { target two_plus_gigs } } */
/* { dg-do compile { target { ! two_plus_gigs } } } */
/* { dg-options "-O3 -save-temps -mcmodel=small" } */

char fixed_regs[0x80000000];

int
main ()
{
  return fixed_regs[0xfffff000];
}

/* { dg-final { scan-assembler-not "adrp\tx\[0-9\]+, fixed_regs\\\+" } } */
