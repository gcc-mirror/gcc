/* { dg-require-effective-target arm_eabi } */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "ldr\[\\t \]+\[^\n\]*,\[\\t \]*\\\[\[^\n\]*\\\]" 2 } } */
/* { dg-final { scan-assembler-times "str\[\\t \]+\[^\n\]*,\[\\t \]*\\\[\[^\n\]*\\\]" 2 } } */
/* { dg-final { scan-assembler-not "strb" } } */

struct thing {
  unsigned a: 8;
  unsigned b: 8;
  unsigned c: 8;
  unsigned d: 8;
};

struct thing2 {
  volatile unsigned a: 8;
  volatile unsigned b: 8;
  volatile unsigned c: 8;
  volatile unsigned d: 8;
};

void test1(volatile struct thing *t)
{
  t->a = 5;
}

void test2(struct thing2 *t)
{
  t->a = 5;
}
