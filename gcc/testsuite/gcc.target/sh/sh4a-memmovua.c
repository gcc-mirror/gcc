/* Verify that we generate movua to copy unaligned memory regions to
   32-bit-aligned addresses on SH4A.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a" "-m4a-single" "-m4a-single-only" "-m4a-nofpu" } }  */
/* { dg-final { scan-assembler-times "movua.l" 2 } } */

#include <string.h>

struct s { int i; char a[10], b[10]; } x;
int f() {
  memcpy(x.a, x.b, 10);
}

