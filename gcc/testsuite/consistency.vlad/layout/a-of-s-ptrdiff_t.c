#include <stdio.h>
#include <stddef.h>

static struct sss{
  ptrdiff_t f;
} a[10];

int main (void) {
  printf ("++++Array of struct with ptrdiff_t:\n");
  printf ("size=%d,align=%d,displ-a[5]=%d,align-a[5]=%d\n",
          sizeof (a), __alignof__ (a), (char*)&a[5] - (char*)a, __alignof__ (a[5]));
  return 0;
}
