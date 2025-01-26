/* { dg-do compile } */
/* { dg-options "-O2 -march=z14 -mzarch" } */

#include "isfinite-isinf-isnormal-signbit.h"

/* { dg-final { scan-assembler-times {wftcixb\t%v[0-9]+,%v[0-9]+,1365} 1 } } */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,1365} 1 } } */
/* { dg-final { scan-assembler-times {wftcixb\t%v[0-9]+,%v[0-9]+,4032} 1 } } */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,4032} 1 } } */
/* { dg-final { scan-assembler-times {wftcixb\t%v[0-9]+,%v[0-9]+,48} 1 } } */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,48} 1 } } */
/* { dg-final { scan-assembler-times {wftcixb\t%v[0-9]+,%v[0-9]+,768} 1 } } */
/* { dg-final { scan-assembler-times {tdcxt\t%f[0-9]+,192} 1 } } */
