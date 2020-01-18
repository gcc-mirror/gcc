/* { dg-do run } */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=0x00400000" } */

typedef struct
{
  unsigned char	  a;
  unsigned short  b;
} test_st;

test_st __attribute__ ((cmse_nonsecure_entry)) foo (void)
{
  test_st t;
  t.a = 255u;
  t.b = 32767u;
  return t;
}

int
main (void)
{
  test_st t;
  t = foo ();
  if (t.a != 255u || t.b != 32767u)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler "vstr\tFPCXTNS, \\\[sp, #-4\\\]!" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "movs\tr1, #255" } } */
/* { dg-final { scan-assembler "movt\tr1, 65535" } } */
/* { dg-final { scan-assembler "ands\tr0(, r0)?, r1" } } */
/* { dg-final { scan-assembler "vscclrm\t\{s0-s15, VPR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "clrm\t\{r1, r2, r3, ip, APSR\}" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "vldr\tFPCXTNS, \\\[sp\\\], #4" { target arm_cmse_clear_ok } } } */
/* { dg-final { scan-assembler "bxns" } } */
