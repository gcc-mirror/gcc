#include <stdio.h>
#include <stddef.h>

static struct sss{
  ptrdiff_t f;
  wchar_t snd;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("+++Struct ptrdiff_t-wchar_t:\n");
  printf ("size=%d,align=%d,offset-ptrdiff_t=%d,offset-wchar_t=%d,\nalign-ptrdiff_t=%d,align-wchar_t=%d\n",
          sizeof (sss), __alignof__ (sss),
          _offsetof (struct sss, f), _offsetof (struct sss, snd),
          __alignof__ (sss.f), __alignof__ (sss.snd));
  return 0;
}
