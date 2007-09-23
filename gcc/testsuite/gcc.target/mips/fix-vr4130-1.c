/* { dg-do compile } */
/* { dg-mips-options "-march=vr4130 -mfix-vr4130" } */
NOMIPS16 int foo (void) { int r; asm ("# foo" : "=h" (r)); return r; }
/* { dg-final { scan-assembler "\tmacchi\t" } } */
