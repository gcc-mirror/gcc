/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "macro.h"

CMP_VI (ne_char, char, n, !=, -16)
CMP_VI (ne_short, short, n, !=, -16)
CMP_VI (ne_int, int, n, !=, -16)
CMP_VI (ne_long, long, n, !=, -16)
CMP_VI (ne_unsigned_char, unsigned char, n, !=, -16)
CMP_VI (ne_unsigned_short, unsigned short, n, !=, -16)
CMP_VI (ne_unsigned_int, unsigned int, n, !=, -16)
CMP_VI (ne_unsigned_long, unsigned long, n, !=, -16)

/* { dg-final { scan-assembler-times {vmsne\.vi} 13 } } */
/* { dg-final { scan-assembler-not {vmsne\.vv} } } */
