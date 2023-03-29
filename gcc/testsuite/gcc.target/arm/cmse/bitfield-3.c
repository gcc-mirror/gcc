/* This test is executed only if the execution engine supports CMSE instructions.  */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=[cmse_sgstubs]" } */

typedef struct
{
  short	      a;
  signed char b : 2;
  short		: 1;
  signed char c : 3;
} test_st;

test_st __attribute__ ((cmse_nonsecure_entry)) foo (void)
{
  test_st t;
  t.a = -32768;
  t.b = -2;
  t.c = -4;
  return t;
}

int
main (void)
{
  test_st t;
  t = foo ();
  if (t.a != -32768
      || t.b != -2
      || t.c != -4)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "movw\tr1, #65535" } } */
/* { dg-final { scan-assembler "movt\tr1, 63" } } */
/* { dg-final { scan-assembler "ands\tr0(, r0)?, r1" } } */
/* { dg-final { scan-assembler "vscclrm\t\{s0-s15, VPR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "clrm\t\{r1, r2, r3, ip, APSR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "bxns" } } */
