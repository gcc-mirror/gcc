#include <stdio.h>

static struct sss{
  char f;
  long long :0;
  int i;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++longlong zerofield inside struct starting with char:\n");
  printf ("size=%d,align=%d\n", sizeof (sss), __alignof__ (sss));
  printf ("offset-char=%d,offset-last=%d,\nalign-char=%d,align-last=%d\n",
          _offsetof (struct sss, f), _offsetof (struct sss, i),
          __alignof__ (sss.f), __alignof__ (sss.i));
  return 0;
}
