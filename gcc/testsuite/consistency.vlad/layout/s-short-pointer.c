#include <stdio.h>

static struct sss{
  short f;
  char * snd;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++Struct short-pointer:\n");
  printf ("size=%d,align=%d,offset-short=%d,offset-pointer=%d,\nalign-short=%d,align-pointer=%d\n",
          sizeof (sss), __alignof__ (sss),
          _offsetof (struct sss, f), _offsetof (struct sss, snd),
          __alignof__ (sss.f), __alignof__ (sss.snd));
  return 0;
}
