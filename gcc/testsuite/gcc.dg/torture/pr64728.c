/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */

#include <setjmp.h>

jmp_buf a;
int b, d;
void baz (long);

static void
bar (long *x)
{
  if (d)
    *x = b;
}

void
foo ()
{
  baz (0);
  if (setjmp (a))
    {
      long c;
      bar (&c);
      baz (c);
    }
  baz (0);
}
