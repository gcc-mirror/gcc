/* { dg-do run } */

#include <stdlib.h>

char c = 42;

void __attribute__((noinline,noclone))
pr39633 (char a)
{
  a >>= 7;
  if (a)
    c = a;
}

int main()
{
  pr39633 (6);

  if (c != 42)
    abort();

  exit(0);
    
  return 0;
}
