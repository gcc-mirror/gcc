/* { dg-do compile } */
/* { dg-options "-O2 -march=z9-ec -mzarch" } */

#define SIGNBIT(T) \
  int signbit_##T (T x) { return __builtin_signbit (x); }

SIGNBIT (float)
/* { dg-final { scan-assembler-times {tceb\t%f[0-9]+,1365} 1 } } */

SIGNBIT (double)
/* { dg-final { scan-assembler-times {tcdb\t%f[0-9]+,1365} 1 } } */

SIGNBIT (_Decimal32)
/* { dg-final { scan-assembler-times {tdcet\t%f[0-9]+,1365} 1 } } */

SIGNBIT (_Decimal64)
/* { dg-final { scan-assembler-times {tdcdt\t%f[0-9]+,1365} 1 } } */

#define ISFINITE(T) \
  int isfinite_##T (T x) { return __builtin_isfinite (x); }

ISFINITE (float)
/* { dg-final { scan-assembler-times {tceb\t%f[0-9]+,4032} 1 } } */

ISFINITE (double)
/* { dg-final { scan-assembler-times {tcdb\t%f[0-9]+,4032} 1 } } */

ISFINITE (_Decimal32)
/* { dg-final { scan-assembler-times {tdcet\t%f[0-9]+,4032} 1 } } */

ISFINITE (_Decimal64)
/* { dg-final { scan-assembler-times {tdcdt\t%f[0-9]+,4032} 1 } } */

#define ISINF(T) \
  int isinf_##T (T x) { return __builtin_isinf (x); }

ISINF (float)
/* { dg-final { scan-assembler-times {tceb\t%f[0-9]+,48} 1 } } */

ISINF (double)
/* { dg-final { scan-assembler-times {tcdb\t%f[0-9]+,48} 1 } } */

ISINF (_Decimal32)
/* { dg-final { scan-assembler-times {tdcet\t%f[0-9]+,48} 1 } } */

ISINF (_Decimal64)
/* { dg-final { scan-assembler-times {tdcdt\t%f[0-9]+,48} 1 } } */

#define ISNORMAL(T) \
  int isnormal_##T (T x) { return __builtin_isnormal (x); }

ISNORMAL (float)
/* { dg-final { scan-assembler-times {tceb\t%f[0-9]+,768} 1 } } */

ISNORMAL (double)
/* { dg-final { scan-assembler-times {tcdb\t%f[0-9]+,768} 1 } } */

ISNORMAL (_Decimal32)
/* { dg-final { scan-assembler-times {tdcet\t%f[0-9]+,192} 1 } } */

ISNORMAL (_Decimal64)
/* { dg-final { scan-assembler-times {tdcdt\t%f[0-9]+,192} 1 } } */
