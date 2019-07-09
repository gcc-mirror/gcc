/* { dg-do run } */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=0x00400000" } */

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

/* { dg-final { scan-assembler "movw\tr1, #65535" } } */
/* { dg-final { scan-assembler "movt\tr1, 63" } } */
/* { dg-final { scan-assembler "ands\tr0(, r0)?, r1" } } */
/* { dg-final { scan-assembler "bxns" } } */

