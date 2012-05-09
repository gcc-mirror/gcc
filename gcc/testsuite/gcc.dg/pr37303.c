/* { dg-do compile { target *-*-elf* *-*-linux-gnu* } } */
/* { dg-options "-std=c99" }
/* { dg-final { scan-assembler "rodata" } } */

struct S { const int *x; } s = { (const int[]){1, 2, 3} };
