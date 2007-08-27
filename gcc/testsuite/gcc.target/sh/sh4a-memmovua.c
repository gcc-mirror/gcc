/* Verify that we generate movua to copy unaligned memory regions to
   32-bit-aligned addresses.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-times "\tmovua\\.l\t(.*)+" 2 } } */

#ifdef __SH4A__
#include <stdlib.h>

struct s { int i; char a[10], b[10]; } x;
int f() {
  memcpy(x.a, x.b, 10);
}
#else
asm ("movua.l\t+");
asm ("movua.l\t+");
#endif
