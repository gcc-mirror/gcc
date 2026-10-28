/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>

jmp_buf jb;
void use (double);
void g (_Complex double *p)
{
  double r = 0;
  _Complex double c = *p;
  setjmp (jb);
  use (r);
  r = __real__ c;
  use (r);
}
