#include <string.h>
#include <stdio.h>

void
a(void *x,int y)
{
  if (y != 1234)
    abort ();
}

int
main()
{
  a(strcpy(alloca(100),"abc"),1234);
  return 0;
}
