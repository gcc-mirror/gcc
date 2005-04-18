/* { dg-do compile } */
/* { dg-mips-options "-march=vr4130 -mfix-vr4130" } */
int foo (void) { int r; asm ("# foo" : "=l" (r)); return r; }
/* { dg-final { scan-assembler "\tmacc\t" } } */
