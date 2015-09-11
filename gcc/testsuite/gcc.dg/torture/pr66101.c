/* { dg-do compile } */
/* { dg-require-effective-target nonlocal_goto } */

#include <setjmp.h>

jmp_buf env;

int a, c, d, e;

int
bar ()
{
  int b = *(long *) 7 == 5 ? : 0;
  if (a || a > b)
    longjmp (env, 0);
  return 1;
}

void
foo ()
{
  long f;
  setjmp (env);
  for (; d; c++)
    switch (c)
case 0:
      {
	f = bar () >> 1;
	if (e)
	  goto L;
	if (bar () >> 1)
	  goto L;
      }
L:
    ;
}
