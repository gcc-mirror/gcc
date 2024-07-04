/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "../vls-vlmax/perm-6.c"

/* { dg-final { scan-assembler-times {vrgather\.vi\tv[0-9]+,\s*v[0-9]+,\s*1} 31 } } */
