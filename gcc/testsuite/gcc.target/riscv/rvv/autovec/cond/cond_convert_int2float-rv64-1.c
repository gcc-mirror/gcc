/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math" } */

#include "cond_convert_int2float-1.h"

/* { dg-final { scan-assembler-times {\tvfcvt\.f\.xu\.v\tv[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvfcvt\.f\.x\.v\tv[0-9]+,v[0-9]+,v0\.t} 3 } } */

/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.xu\.v\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.x\.v\tv[0-9]+,v[0-9]+,v0\.t} 4 } } */

/* { dg-final { scan-assembler-times {\tvfncvt\.f\.xu\.w\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvfncvt\.f\.x\.w\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */

/* { dg-final { scan-assembler-times {\tvfncvt\.f\.f\.w\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */

/* { dg-final { scan-assembler {\tvsetvli\t[a-z0-9]+,[a-z0-9]+,e[0-9]+,m[f0-9]+,t[au],mu} } } */

/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
