/* { dg-do run } */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=0x00400000" } */

typedef struct
{
  short	      a : 7;
  signed char b : 3;
  short	      c : 11;
} test_st;

test_st __attribute__ ((cmse_nonsecure_entry)) foo (void)
{
  test_st t;
  t.a = -64;
  t.b = -4 ;
  t.c = -1024;
  return t;
}

int
main (void)
{
  test_st t;
  t = foo ();
  if (t.a != -64
      || t.b != -4
      || t.c != -1024)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler "movw\tr1, #1919" } } */
/* { dg-final { scan-assembler "movt\tr1, 2047" } } */
/* { dg-final { scan-assembler "ands\tr0(, r0)?, r1" } } */
/* { dg-final { scan-assembler "bxns" } } */

