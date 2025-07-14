/* { dg-do compile { target { fpic && lp64 } } } */
/* { dg-options "-O2 -march=x86-64 -fpic -mcmodel=large" } */

#include "pr121062-3a.c"

/* { dg-final { scan-assembler-times "movq\[ \\t\]+\\\$-1, \\(%r\[a-z0-9\]+\\)" 1 } } */
