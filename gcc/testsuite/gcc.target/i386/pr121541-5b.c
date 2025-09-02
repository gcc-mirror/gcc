/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

#include "pr121541-5a.c"

/* { dg-final { scan-assembler "call\[\\t \]+_?__mulxf3" } } */
