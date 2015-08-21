/* { dg-skip-if "requires io" { freestanding } { "*" } { "" } }  */

#include <stdio.h>
#include <stdlib.h>

int
main (void)
{
#define test(ret, args...) \
  printf (args); 		\
  if (printf (args) != ret)	\
    abort ();
  test (5, "hello");
  test (6, "hello\n");
  test (1, "a");
  test (0, "");
  test (5, "%s", "hello");
  test (6, "%s", "hello\n");
  test (1, "%s", "a");
  test (0, "%s", "");
  test (1, "%c", 'x');
  test (7, "%s\n", "hello\n");
  test (2, "%d\n", 0);
  return 0;
}
