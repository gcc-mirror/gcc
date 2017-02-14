/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mfloat128" } */

__float128 f128_add (__float128 a, __float128 b) { return a+b; }
__float128 f128_sub (__float128 a, __float128 b) { return a-b; }
__float128 f128_mul (__float128 a, __float128 b) { return a*b; }
__float128 f128_div (__float128 a, __float128 b) { return a/b; }
__float128 f128_fma (__float128 a, __float128 b, __float128 c) { return (a*b)+c; }
long f128_cmove (__float128 a, __float128 b, long c, long d) { return (a == b) ? c : d; }

/* { dg-final { scan-assembler "xsaddqp"  } } */
/* { dg-final { scan-assembler "xssubqp"  } } */
/* { dg-final { scan-assembler "xsmulqp"  } } */
/* { dg-final { scan-assembler "xsdivqp"  } } */
/* { dg-final { scan-assembler "xsmaddqp" } } */
/* { dg-final { scan-assembler "xscmpuqp" } } */
