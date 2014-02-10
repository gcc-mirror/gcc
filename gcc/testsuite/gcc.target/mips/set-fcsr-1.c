/* { dg-options "-mhard-float" } */
/* { dg-skip-if "requiring \$4 is a code-quality test" { *-*-* } { "-O0" } { "" } } */

NOMIPS16 void
foo (unsigned int x)
{
  __builtin_mips_set_fcsr (x);
}

/* { dg-final { scan-assembler "ctc1\t\\\$4,\\\$31" } } */
