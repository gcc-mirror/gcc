/* { dg-do compile } */
/* { dg-options "-O -mgeneral-regs-only -msse" } */

typedef int __attribute__((__vector_size__ (8))) V;
V a, b;
int c;

void
foo (void)
{
  b = 0 != a | b << c;
}
