/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -Wall -Wno-unused-variable" } */

#include <stdio.h>

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  const int t = 0;
  struct s {
    int x;
    /* Don't instrument this one.  */
    int g[t];
  };

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
