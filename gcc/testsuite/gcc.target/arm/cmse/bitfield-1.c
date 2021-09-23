/* This test is executed only if the execution engine supports CMSE instructions.  */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=0x00400000" } */

typedef struct
{
  unsigned short  a : 6;
  unsigned char	  b : 3;
  unsigned char	  c;
  unsigned short  d : 8;
} test_st;

test_st __attribute__ ((cmse_nonsecure_entry)) foo (void)
{
  test_st t;
  t.a = 63u;
  t.b = 7u;
  t.c = 255u;
  t.d = 255u;
  return t;
}

int
main (void)
{
  test_st t;
  t = foo ();
  if (t.a != 63u
      || t.b != 7u
      || t.c != 255u
      || t.d != 255u)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "movw\tr1, #1855" } } */
/* { dg-final { scan-assembler "movt\tr1, 65535" } } */
/* { dg-final { scan-assembler "ands\tr0(, r0)?, r1" } } */
/* { dg-final { scan-assembler "vscclrm\t\{s0-s15, VPR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "clrm\t\{r1, r2, r3, ip, APSR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "bxns" } } */

