/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test return value: small VLS types should be returned in GPRs.  */

typedef int __attribute__((vector_size(8))) v2si;

v2si make_vls (void)
{
  v2si v = {1, 2};
  return v;
}

/* The return value should use a0.  */
/* { dg-final { scan-assembler "ld\ta0," } } */
