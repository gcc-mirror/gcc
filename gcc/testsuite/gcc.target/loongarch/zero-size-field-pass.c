/* Test that LoongArch backend ignores zero-sized fields of aggregates in
   argument passing.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mdouble-float -mabi=lp64d" } */
/* { dg-final { scan-assembler "\\\$f1" } } */

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

extern void callee (struct test);

void
caller (void)
{
  struct test test;
  test.x = 114;
  test.y = 514;
  callee (test);
}
