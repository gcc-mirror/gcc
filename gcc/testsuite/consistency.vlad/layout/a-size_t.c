#include <stdio.h>
#include <stddef.h>

static size_t a [10];
static size_t e [0]; /* GCC only */

int main (void) {
  printf ("+++Array size_t:\n");
  printf ("size=%d,align=%d,5th-elem-offset=%d,5th-elem-align=%d\n",
          sizeof (a), __alignof__ (a),
          (char *) &a[5] - (char *) a, __alignof__ (a[5]));
  printf ("size=%d,align=%d,5th-elem-offset=%d,5th-elem-align=%d\n",
          sizeof (e), __alignof__ (e),
          (char *) &e[5] - (char *) a, __alignof__ (e[5]));
  return 0;
}
