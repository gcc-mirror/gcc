/* { dg-options "-mhard-float" } */

NOMIPS16 unsigned int
foo (void)
{
  return __builtin_mips_get_fcsr ();
}

/* { dg-final { scan-assembler "cfc1\t\\\$2,\\\$31" } } */
