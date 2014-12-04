/* PR tree-optimization/59920 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

int bar (void);
void baz (int);

#define A { int x = bar (); if (setjmp (buf) == 0) baz (x); }
#define B A A A A A A A A A A
#define C B B B B B B B B B B

extern jmp_buf buf;

void
foo (void)
{
  C C
}
