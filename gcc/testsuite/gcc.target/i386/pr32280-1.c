/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

__uint128_t
t1 (__uint128_t a)
{
  return a << 8;
}

__uint128_t
t2 (__uint128_t a)
{
  return a >> 8;
}

/* { dg-final { scan-assembler-not "pslldq" } } */
/* { dg-final { scan-assembler-not "psrldq" } } */
