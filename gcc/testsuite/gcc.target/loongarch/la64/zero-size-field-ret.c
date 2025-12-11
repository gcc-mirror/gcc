/* Test that LoongArch backend ignores zero-sized fields of aggregates in
   returning.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mdouble-float -mabi=lp64d" } */
/* { dg-final { scan-assembler-not "\\\$r4" } } */

struct test
{
  int empty1[0];
  double empty2[0];
  int : 0;
  float x;
  long empty3[0];
  long : 0;
  float y;
  unsigned : 0;
  char empty4[0];
};

extern struct test callee (void);

float
caller (void)
{
  struct test test = callee ();
  return test.x + test.y;
}
