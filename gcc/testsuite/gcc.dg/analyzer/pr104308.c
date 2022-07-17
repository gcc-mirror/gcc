/* Verify that we have source locations for
   -Wanalyzer-use-of-uninitialized-value warnings involving folded
   memory ops.  */

#include <string.h>

int test_memmove_within_uninit (void)
{
  char s[5]; /* { dg-message "region created on stack here" } */
  memmove(s, s + 1, 2); /* { dg-warning "use of uninitialized value" } */
  return 0;
}

int test_memcpy_from_uninit (void)
{
  char a1[5];
  char a2[5]; /* { dg-message "region created on stack here" } */
  return (memcpy(a1, a2, 5) == a1); /* { dg-warning "use of uninitialized value" } */
}
