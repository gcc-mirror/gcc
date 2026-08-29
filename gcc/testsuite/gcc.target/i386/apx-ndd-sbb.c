/* { dg-do compile { target int128 } } */
/* { dg-options "-mapxf -mtune-ctrl=enable_ndd_mem -O2" } */

#include "pr91681-2.c"

/* { dg-final { scan-assembler-times "sbbq\[^\n\r]*0, %rdi, %rdx" 1 } } */
