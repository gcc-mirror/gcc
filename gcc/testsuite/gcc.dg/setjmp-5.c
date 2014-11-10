/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

void bar (int);

jmp_buf buf;
int v;

void
foo (void)
{
  int i;
  bar (0);
  bar (1);
  i = 5;
  int j = setjmp (buf);
  if (j == 0)
    bar (2);
  v = i;	/* { dg-bogus "may be used uninitialized in this function" } */
}
