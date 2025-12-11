/* { dg-do compile } */
/* { dg-options "-mexplicit-relocs -mdirect-extern-access" } */
/* { dg-final { scan-assembler-not "got" } } */

extern int x;
int f() { return x; }
