/* { dg-do run } */
/* { dg-additional-options "-O3" } */

#include <stdlib.h>

int a;
int c;
__attribute__((noinline, noclone)) void foo (int x)
{
  if (x == 0)
    c++;
}

volatile int t = 1;

int
main (int argc, char* argv[])
{
  int j, k, b = 0;
  if (t == 0)
    b = 1;
  for (j = 0; j < 3; j++)
    for (k = 0; k < 1; k++)
      {
	foo (0);
	if (b)
	  for (k = -1; a;)
	    ;
      }
  if (c != 3)
    abort ();
  return 0;
}
