/* Test that the compiler properly optimizes conditional floating point moves
   into the pcmov instruction on XOP systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mxop" } */

extern void exit (int);

float flt_test (float a, float b, float c, float d)
{
  return (a > b) ? c : d;
}

float flt_a = 1, flt_b = 2, flt_c = 3, flt_d = 4, flt_e;

int main()
{
  flt_e = flt_test (flt_a, flt_b, flt_c, flt_d);
  exit (0);
}

/* { dg-final { scan-assembler "vpcmov" } } */
