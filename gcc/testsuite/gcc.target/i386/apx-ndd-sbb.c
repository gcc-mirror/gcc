/* { dg-do compile { target { int128 && { ! ia32 } } } } */
/* { dg-options "-mapxf -O2" } */

#include "pr91681-2.c"

/* { dg-final { scan-assembler-times "sbbq\[^\n\r]*0, %rdi, %rdx" 1 } } */
