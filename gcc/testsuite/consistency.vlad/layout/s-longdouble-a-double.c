#include <stdio.h>

static struct sss{
  long double f;
  double a[10];
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("++++Array of double in struct starting with longdouble:\n");
  printf ("size=%d,align=%d\n",
          sizeof (sss), __alignof__ (sss));
  printf ("offset-longdouble=%d,offset-arrayof-double=%d,\nalign-longdouble=%d,align-arrayof-double=%d\n",
          _offsetof (struct sss, f), _offsetof (struct sss, a),
          __alignof__ (sss.f), __alignof__ (sss.a));
  printf ("offset-double-a[5]=%d,align-double-a[5]=%d\n",
          _offsetof (struct sss, a[5]),
          __alignof__ (sss.a[5]));
  return 0;
}
