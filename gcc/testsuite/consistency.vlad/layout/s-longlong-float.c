#include <stdio.h>

static struct sss{
  long long f;
  float snd;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++Struct longlong-float:\n");
  printf ("size=%d,align=%d,offset-longlong=%d,offset-float=%d,\nalign-longlong=%d,align-float=%d\n",
          sizeof (sss), __alignof__ (sss),
          _offsetof (struct sss, f), _offsetof (struct sss, snd),
          __alignof__ (sss.f), __alignof__ (sss.snd));
  return 0;
}
