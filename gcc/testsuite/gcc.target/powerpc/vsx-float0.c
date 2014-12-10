/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler "xxlxor" } } */

/* Test that we generate xxlor to clear a SFmode register.  */

float sum (float *p, unsigned long n)
{
  float sum = 0.0f;	/* generate xxlxor instead of load */
  while (n-- > 0)
    sum += *p++;

  return sum;
}
