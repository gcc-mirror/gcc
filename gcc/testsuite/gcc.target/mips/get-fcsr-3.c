/* { dg-options "-mabi=64 -mhard-float" } */

NOMIPS16 unsigned int
foo (void)
{
  return __builtin_mips_get_fcsr () & 0x1;
}

/* { dg-final { scan-assembler "cfc1" } } */
