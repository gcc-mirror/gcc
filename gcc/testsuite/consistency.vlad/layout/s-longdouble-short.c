#include <stdio.h>

static struct sss{
  long double f;
  short snd;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++Struct longdouble-short:\n");
  printf ("size=%d,align=%d,offset-longdouble=%d,offset-short=%d,\nalign-longdouble=%d,align-short=%d\n",
          sizeof (sss), __alignof__ (sss),
          _offsetof (struct sss, f), _offsetof (struct sss, snd),
          __alignof__ (sss.f), __alignof__ (sss.snd));
  return 0;
}
