/* Regression test for paste corner cases.  Distilled from
   syscall stub logic in glibc.  */

/* { dg-do run } */

#include <stdlib.h>

#define ENTRY(name)	name##:
#define socket bind

int
main(void)
{
  goto socket;

  ENTRY(socket)
    return 0;
}
