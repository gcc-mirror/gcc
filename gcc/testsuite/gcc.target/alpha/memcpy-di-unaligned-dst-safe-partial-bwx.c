/* { dg-do compile } */
/* { dg-options "-msafe-partial -mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memcpy-di-unaligned-dst.c"

/* { dg-final { scan-assembler-times "\\sldq\\s" 7 } } */
/* { dg-final { scan-assembler-times "\\sstb\\s" 16 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 6 } } */
/* { dg-final { scan-assembler-not "\\sldq_l\\s" } } */
/* { dg-final { scan-assembler-not "\\sldq_u\\s" } } */
/* { dg-final { scan-assembler-not "\\sstq\\s" } } */
/* { dg-final { scan-assembler-not "\\sstq_c\\s" } } */
