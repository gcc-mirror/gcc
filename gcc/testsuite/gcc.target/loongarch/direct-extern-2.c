/* { dg-do compile } */
/* { dg-options "-mno-explicit-relocs -mdirect-extern-access" } */
/* { dg-final { scan-assembler-not "la.global" } } */

extern int x;
int f() { return x; }
