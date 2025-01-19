/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "memcpy-si-unaligned-src.c"

/* { dg-final { scan-assembler-times "\\sldbu\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sldq_u\\s" 8 } } */
/* { dg-final { scan-assembler-times "\\sstb\\s" 4 } } */
/* { dg-final { scan-assembler-times "\\sstl\\s" 14 } } */
/* { dg-final { scan-assembler-not "\\s(?:ldl|stq_u)\\s" } } */
