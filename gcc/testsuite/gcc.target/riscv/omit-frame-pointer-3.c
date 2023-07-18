/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2 -fomit-frame-pointer -mno-omit-leaf-frame-pointer -fno-inline" } */

#include "omit-frame-pointer-test.c"

/* { dg-final { scan-assembler-times "sd\tra,\[0-9\]+\\(sp\\)" 1 } } */
/* { dg-final { scan-assembler-not "sd\ts0,\[0-9\]+\\(sp\\)"} } */
