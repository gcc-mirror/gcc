/* Large static storage.  */

#include <limits.h>

static volatile char chars_1[INT_MAX / 2];
static volatile char chars_2[1];

int
foo (void)
{
  chars_1[10] = 'y';
  chars_2[0] = 'x';
}
