/* { dg-lto-do run } */
#include <stdio.h>

extern int foo (int);

int
main()
{
  int x = foo (10);
  printf ("x is %d, foo is at 0x%p\n", x, foo);
  return 0;
}
