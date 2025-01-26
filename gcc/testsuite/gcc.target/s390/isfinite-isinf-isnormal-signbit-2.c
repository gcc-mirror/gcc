/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */

#include "isfinite-isinf-isnormal-signbit.h"

/* { dg-final { scan-assembler-times {tcxb\t%f[0-9]+,1365} 1 } } SIGNBIT long double */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,1365} 1 } } SIGNBIT _Decimal128 */
/* { dg-final { scan-assembler-times {tcxb\t%f[0-9]+,4032} 1 } } ISFINITE long double */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,4032} 1 } } ISFINITE _Decimal128 */
/* { dg-final { scan-assembler-times {tcxb\t%f[0-9]+,48} 1 } } ISINF long double */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,48} 1 } } ISINF _Decimal128 */
/* { dg-final { scan-assembler-times {tcxb\t%f[0-9]+,768} 1 } } ISNORMAL long double */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,192} 1 } } ISNORMAL _Decimal128 */
