/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "macro.h"

CMP_VI (le_char, char, n, <=, 15)
CMP_VI (le_short, short, n, <=, 15)
CMP_VI (le_int, int, n, <=, 15)
CMP_VI (le_long, long, n, <=, 15)
CMP_VI (le_unsigned_char, unsigned char, n, <=, 15)
CMP_VI (le_unsigned_short, unsigned short, n, <=, 15)
CMP_VI (le_unsigned_int, unsigned int, n, <=, 15)
CMP_VI (le_unsigned_long, unsigned long, n, <=, 15)

/* { dg-final { scan-assembler-times {vmsle\.vi} 7 { target {
     any-opts "-mrvv-max-lmul=m1"
   } } } } */
/* { dg-final { scan-assembler-times {vmsleu\.vi} 9 { target {
     any-opts "-mrvv-max-lmul=m1"
   } } } } */

/* { dg-final { scan-assembler-times {vmsle\.vi} 4 { target {
     any-opts "-mrvv-max-lmul=m2"
   } } } } */
/* { dg-final { scan-assembler-times {vmsleu\.vi} 6 { target {
     any-opts "-mrvv-max-lmul=m2"
   } } } } */

/* { dg-final { scan-assembler-times {vmsle\.vi} 3 { target {
     any-opts "-mrvv-max-lmul=m4" "-mrvv-max-lmul=m8" "-mrvv-max-lmul=dynamic"
   } } } } */
/* { dg-final { scan-assembler-times {vmsleu\.vi} 5 { target {
     any-opts "-mrvv-max-lmul=m4" "-mrvv-max-lmul=m4" "-mrvv-max-lmul=dynamic"
   } } } } */

/* { dg-final { scan-assembler-not {vmsle\.vv} } } */
/* { dg-final { scan-assembler-not {vmsleu\.vv} } } */
