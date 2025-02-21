/* PR target/83368 */
/* Testcase written by James Clarke <jrtc27@jrtc27.com> */

/* { dg-do run { target *-*-solaris2.* *-*-linux* *-*-*bsd* } } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */

#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>

jmp_buf jb;

int foo = 99;
int c = 0;

void bar (void)
{
  c++;
  longjmp (jb, 1);
}

int main (void)
{
  setjmp (jb);

  char *p = __builtin_alloca (256);
  memset (p, 0, 256);
  sprintf (p, "%d\n", foo);

  if (c < 10)
    bar();

  return 0;
}
