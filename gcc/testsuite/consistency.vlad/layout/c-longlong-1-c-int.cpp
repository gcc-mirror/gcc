#include <stdio.h>

class c{
public:
  long long f;
};


static class sss: public c{
public:
  int m;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("++Class with int inhereting class with longlong:\n");
  printf ("size=%d,align=%d\n", sizeof (sss), __alignof__ (sss));
  printf ("offset-longlong=%d,offset-int=%d,\nalign-longlong=%d,align-int=%d\n",
          _offsetof (class sss, f), _offsetof (class sss, m),
          __alignof__ (sss.f), __alignof__ (sss.m));
  return 0;
}
