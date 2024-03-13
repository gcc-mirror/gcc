/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "../vls-vlmax/perm-4.c"

/* { dg-final { scan-assembler-times {vrgather\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-times {vrgatherei16\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 12 } } */
/* { dg-final { scan-assembler-times {vrsub\.vi} 24 } } */
/* { dg-final { scan-assembler-times {vrsub\.vx} 7 } } */
