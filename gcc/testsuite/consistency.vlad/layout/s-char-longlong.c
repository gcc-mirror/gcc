#include <stdio.h>

static struct sss{
  char f;
  long long snd;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++Struct char-longlong:\n");
  printf ("size=%d,align=%d,offset-char=%d,offset-longlong=%d,\nalign-char=%d,align-longlong=%d\n",
          sizeof (sss), __alignof__ (sss),
          _offsetof (struct sss, f), _offsetof (struct sss, snd),
          __alignof__ (sss.f), __alignof__ (sss.snd));
  return 0;
}
