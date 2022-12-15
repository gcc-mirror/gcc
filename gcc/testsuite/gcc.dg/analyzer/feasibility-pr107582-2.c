#include <stdio.h>
#include <stdlib.h>

#include "analyzer-decls.h"

int z;

static void func(void)
{
}

int main(int    argc,
         int ** argv)
{
  int * x;
  int   ret = 0;
  void (*callback) () = func;

  while (ret != 110)
    ret = rand() % 1000;

  if (ret != 110)
    x = &z;

  (*callback) ();

  if (ret == 110)
    return 0;

  __analyzer_dump_path (); /* { dg-bogus "path" } */
  printf("x = %d\n", *x); /* { dg-bogus "use of uninitialized value 'x'" } */

  return 0;
}
