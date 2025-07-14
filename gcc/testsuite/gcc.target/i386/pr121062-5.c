/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

typedef int __attribute__((__vector_size__ (4))) S;

void
foo (S *c)
{
  *c = (S){0x12345678};
}


/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$305419896, \\(%(e|r)\[a-z0-9\]+\\)" 1 } } */
