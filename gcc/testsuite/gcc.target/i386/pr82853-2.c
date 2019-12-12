/* PR middle-end/82853 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-bmi2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "mul\[lq]\t" 2 } } */
/* { dg-final { scan-assembler-not "div\[lq]\t" } } */

unsigned f1 (unsigned x, unsigned *y) { *y = x / 679U; return (x % 679U) == 0; }
