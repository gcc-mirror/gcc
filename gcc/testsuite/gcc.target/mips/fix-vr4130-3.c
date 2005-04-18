/* { dg-do compile } */
/* { dg-mips-options "-march=vr4130 -mgp64 -mfix-vr4130" } */
long long foo (void) { long long r; asm ("# foo" : "=h" (r)); return r; }
/* { dg-final { scan-assembler "\tdmacchi\t" } } */
