#include <stdio.h>

static struct sss{
  long f;
  long double a[10];
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("++++Array of longdouble in struct starting with long:\n");
  printf ("size=%d,align=%d\n",
          sizeof (sss), __alignof__ (sss));
  printf ("offset-long=%d,offset-arrayof-longdouble=%d,\nalign-long=%d,align-arrayof-longdouble=%d\n",
          _offsetof (struct sss, f), _offsetof (struct sss, a),
          __alignof__ (sss.f), __alignof__ (sss.a));
  printf ("offset-longdouble-a[5]=%d,align-longdouble-a[5]=%d\n",
          _offsetof (struct sss, a[5]),
          __alignof__ (sss.a[5]));
  return 0;
}
