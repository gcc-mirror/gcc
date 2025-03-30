/* { dg-do compile } */
/* { dg-options "-msafe-partial -mno-bwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memcpy-di-unaligned-dst.c"

/* { dg-final { scan-assembler-times "\\sldq\\s" 7 } } */
/* { dg-final { scan-assembler-times "\\sldq_l\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\sstq_c\\s" 2 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 6 } } */
/* { dg-final { scan-assembler-not "\\sldq_u\\s" } } */
/* { dg-final { scan-assembler-not "\\sstq\\s" } } */
