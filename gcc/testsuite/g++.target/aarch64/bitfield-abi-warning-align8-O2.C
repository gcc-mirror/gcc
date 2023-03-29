/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector -save-temps -Wno-narrowing" } */

#define ALIGN 8
#define EXTRA

#include "bitfield-abi-warning.h"

/* In f1, f2, f4, f8, fp, f1p, f2p, f4p, f8p (and stdarg versions):  */
/* { dg-final { scan-assembler-times "and\tw0, w1, 1" 18 } } */

/* In f16, f16p (and stdarg versions):  */
/* { dg-final { scan-assembler-times "and\tw0, w2, 1" 4 } } */

/* In f1, f2, f4, f8, f16, fp, f1p, f2p, f4p, f8p, f16p stack versions:  */
/* { dg-final { scan-assembler-times "and\tw0, w0, 1" 11 } } */
