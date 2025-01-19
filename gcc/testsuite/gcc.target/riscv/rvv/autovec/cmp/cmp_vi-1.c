/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "macro.h"

CMP_VI (ne_char, char, n, !=, 15)
CMP_VI (ne_short, short, n, !=, 15)
CMP_VI (ne_int, int, n, !=, 15)
CMP_VI (ne_long, long, n, !=, 15)
CMP_VI (ne_unsigned_char, unsigned char, n, !=, 15)
CMP_VI (ne_unsigned_short, unsigned short, n, !=, 15)
CMP_VI (ne_unsigned_int, unsigned int, n, !=, 15)
CMP_VI (ne_unsigned_long, unsigned long, n, !=, 15)

/* { dg-final { scan-assembler-times {vmsne\.vi} 16 { target {
     any-opts "-mrvv-max-lmul=m1"
   } } } } */

/* { dg-final { scan-assembler-times {vmsne\.vi} 10 { target {
     any-opts "-mrvv-max-lmul=m2"
   } } } } */

/* { dg-final { scan-assembler-times {vmsne\.vi} 8 { target {
     any-opts "-mrvv-max-lmul=m4" "-mrvv-max-lmul=m8" "-mrvv-max-lmul=dynamic"
   } } } } */

/* { dg-final { scan-assembler-not {vmsne\.vv} } } */
