#include <stdio.h>
int
main ()
{
  FILE *f = fopen ("conftest.out", "w");
  if (f == NULL)
    return 1;
  return ferror (f) || fclose (f) != 0;

  ;
  return 0;
}
