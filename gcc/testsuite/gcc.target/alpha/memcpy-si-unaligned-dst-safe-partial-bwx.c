/* { dg-do compile } */
/* { dg-options "-msafe-partial -mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memcpy-si-unaligned-dst.c"

/* { dg-final { scan-assembler-times "\\sldl\\s" 15 } } */
/* { dg-final { scan-assembler-times "\\sstb\\s" 20 } } */
/* { dg-final { scan-assembler-times "\\sstq_u\\s" 6 } } */
/* { dg-final { scan-assembler-not "\\sldq_l\\s" } } */
/* { dg-final { scan-assembler-not "\\sldq_u\\s" } } */
/* { dg-final { scan-assembler-not "\\sstl\\s" } } */
/* { dg-final { scan-assembler-not "\\sstq_c\\s" } } */
