/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh_zvl128b -mabi=lp64d -mrvv-vector-bits=scalable -mrvv-max-lmul=m2 -fno-vect-cost-model -ffast-math" } */

#include "cond_widen_reduc-1.c"

/* { dg-final { scan-assembler-times {\tvfwredusum\.vs\tv[0-9]+,v[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvwredsum\.vs\tv[0-9]+,v[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvwredsumu\.vs\tv[0-9]+,v[0-9]+,v[0-9]+,v0\.t} 3 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf4\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf8\tv[0-9]+,v[0-9]+,v0\.t} 1 } } */
/* { dg-final { scan-assembler-times {\tvzext\.vf4\tv[0-9]+,v[0-9]+,v0\.t} 2 } } */
/* { dg-final { scan-assembler-times {\tvzext\.vf8\tv[0-9]+,v[0-9]+,v0\.t} 1 } } */
