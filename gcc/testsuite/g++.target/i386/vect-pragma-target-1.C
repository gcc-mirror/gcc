/* { dg-do compile { target { i?86-*-* x86_64-*-* } } }                 */
/* { dg-options "-O0" }                                                 */
/* { dg-final { scan-assembler-times "paddd.+xmm\[0-9]+"        1 } }   */
/* { dg-final { scan-assembler-times "vfmadd132ps.+ymm\[0-9]+"  1 } }   */
/* { dg-final { scan-assembler-times "vpaddw.+zmm\[0-9]+"       1 } }   */
#include "../../gcc.target/i386/vect-pragma-target-1.c"
