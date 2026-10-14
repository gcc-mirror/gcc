/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvqwbdota16i_zvfwbdota16bf_zvfqwbdota8f_zvfbdota32f_zvfbfmin_zvl4096b -mabi=lp64d -mrvv-vector-bits=zvl -O2" } */

#include "zvbdota-intrinsic.c"

/* { dg-final { scan-assembler {vfbdota\.vv[^\n]*56} } } */
