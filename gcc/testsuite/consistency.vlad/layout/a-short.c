#include <stdio.h>

static short a [10];
static short e [0]; /* GCC only */

int main (void) {
  printf ("+++Array short:\n");
  printf ("size=%d,align=%d,5th-elem-offset=%d,5th-elem-align=%d\n",
          sizeof (a), __alignof__ (a),
          (char *) &a[5] - (char *) a, __alignof__ (a[5]));
  printf ("size=%d,align=%d,5th-elem-offset=%d,5th-elem-align=%d\n",
          sizeof (e), __alignof__ (e),
          (char *) &e[5] - (char *) a, __alignof__ (e[5]));
  return 0;
}
