/* { dg-do compile } */
/* { dg-options "-msafe-partial -mno-bwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memcpy-si-unaligned-dst.c"

/* { dg-final { scan-assembler-times "\\sldl\\s" 15 } } */
/* { dg-final { scan-assembler-times "\\sldq_l\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sstq_c\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 6 } } */
/* { dg-final { scan-assembler-not "\\sldq_u\\s" } } */
/* { dg-final { scan-assembler-not "\\sstl\\s" } } */
