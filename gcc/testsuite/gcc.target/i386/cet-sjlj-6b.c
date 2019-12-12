/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O -maddress-mode=long -fcf-protection -mx32" } */
/* { dg-final { scan-assembler-times "endbr64" 2 } } */
/* { dg-final { scan-assembler-times "movq\t.*buf\\+16" 1 } } */
/* { dg-final { scan-assembler-times "subq\tbuf\\+16" 1 } } */
/* { dg-final { scan-assembler-times "shrl\t\\\$3," 1 } } */
/* { dg-final { scan-assembler-times "rdsspq" 2 } } */
/* { dg-final { scan-assembler-times "incsspq" 2 } } */

#include "cet-sjlj-6a.c"
