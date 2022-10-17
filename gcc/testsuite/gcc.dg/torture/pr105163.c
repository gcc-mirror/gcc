/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */

#include <setjmp.h>

extern int bar (unsigned int *);
extern jmp_buf *baz (void);
struct C { int c1; };
void foo (struct C *x, int *z, int e)
{
  unsigned int d = 0;
  long f;
  setjmp (*baz());
  f = 1 + ~d;
  d = 8;
  if ((!0) && !e && bar(z)) *z = 1 + f;
}
