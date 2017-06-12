/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

jmp_buf buf;

void foo (int);
void bar (int) __attribute__((leaf));

void enumerate_locals (int indent)
{
  foo (0);
  while (indent--)
    {
      int local_indent = 8 + (8 * indent);
      if (local_indent != 8)
	{
	  setjmp (buf);
	  bar (local_indent);  /* { dg-bogus "may be used uninitialized" } */
	}
    }
  foo (1);
}
