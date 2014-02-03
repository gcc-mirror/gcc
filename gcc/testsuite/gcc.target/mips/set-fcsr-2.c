/* { dg-options "-mhard-float (-mips16)" } */

MIPS16 void
foo (unsigned int x)
{
  __builtin_mips_set_fcsr (x);
}

/* { dg-final { scan-assembler "__mips16_set_fcsr" } } */
/* { dg-final { scan-assembler "ctc1\t\\\$4,\\\$31" } } */
