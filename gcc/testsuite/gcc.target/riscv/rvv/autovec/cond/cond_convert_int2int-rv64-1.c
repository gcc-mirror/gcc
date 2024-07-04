/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_convert_int2int-1.h"

/* { dg-final { scan-assembler-times {\tvzext\.vf2\tv[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf2\tv[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvzext\.vf4\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf4\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvzext\.vf8\tv[0-9]+,v[0-9]+,v0\.t} 1 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf8\tv[0-9]+,v[0-9]+,v0\.t} 1 } } */

/* { dg-final { scan-assembler-times {\tvncvt\.x\.x\.w\tv[0-9]+,v[0-9]+,v0\.t} 12 } } */
/* { dg-final { scan-assembler-times {\tvncvt\.x\.x\.w\tv[0-9]+,v[0-9]+\n} 8 } } */

/* { dg-final { scan-assembler {\tvsetvli\t[a-z0-9]+,[a-z0-9]+,e[0-9]+,m[f0-9]+,t[au],mu} } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
