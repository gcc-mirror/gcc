/* Regression test for a cpplib macro-expansion bug where
   `@' becomes `@@' when stringified.  */

/* { dg-do run } */

#include <string.h>

#define STR(x) #x

char *a = STR(@foo), *b = "@foo";

int
main(void)
{
  if (strcmp (a, b))
    abort ();
  return 0;
}
