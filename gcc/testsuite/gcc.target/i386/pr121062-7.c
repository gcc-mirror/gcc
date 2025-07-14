/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

typedef __bf16 __attribute__((__vector_size__ (4))) S;

void
foo (S *c)
{
  *c = (S){-0.1, 2.1};
}


/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$1074183629, \\(%(e|r)\[a-z0-9\]+\\)" 1 } } */
