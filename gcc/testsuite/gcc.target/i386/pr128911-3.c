/* PR target/128911 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf" } */
/* { dg-final { scan-assembler-times "ctestsl" 2 } } */
/* { dg-final { scan-assembler-times "ctestnsl" 3 } } */
/* { dg-final { scan-assembler-not "ctest(l|ge)l" } } */

#include "pr128911-2.c"
