#include <stdio.h>

static struct sss{
  char f;
  float a[10];
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("++++Array of float in struct starting with char:\n");
  printf ("size=%d,align=%d\n",
          sizeof (sss), __alignof__ (sss));
  printf ("offset-char=%d,offset-arrayof-float=%d,\nalign-char=%d,align-arrayof-float=%d\n",
          _offsetof (struct sss, f), _offsetof (struct sss, a),
          __alignof__ (sss.f), __alignof__ (sss.a));
  printf ("offset-float-a[5]=%d,align-float-a[5]=%d\n",
          _offsetof (struct sss, a[5]),
          __alignof__ (sss.a[5]));
  return 0;
}
