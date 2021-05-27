/* { dg-do compile } */
/* { dg-options "-O" } */

extern void foo(void);

void BNONE_test(int a, int b)
{
  if (a & b)
    foo();
}

void BANY_test(int a, int b)
{
  if (!(a & b))
    foo();
}

void BALL_test(int a, int b)
{
  if (~a & b)
    foo();
}

void BNALL_test(int a, int b)
{
  if (!(~a & b))
    foo();
}

/* { dg-final { scan-assembler-times "bnone" 1 } } */
/* { dg-final { scan-assembler-times "bany" 1 } } */
/* { dg-final { scan-assembler-times "ball" 1 } } */
/* { dg-final { scan-assembler-times "bnall" 1 } } */
