/* { dg-do compile } */
/* { dg-options "-O1 -mthumb -march=armv5t" }  */

struct foo
{
  unsigned b31 : 1;
  unsigned b30 : 1;
  unsigned b29 : 1;
  unsigned b28 : 1;
  unsigned rest : 28;
};
foo(a)
     struct foo a;
{
  return a.b30;
}

/* { dg-final { scan-assembler-times "lsl" 1 } } */
/* { dg-final { scan-assembler-times "lsr" 1 } } */
