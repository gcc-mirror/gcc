#include <stdio.h>

class c{
public:
  long long f;
};

class c2{
public:
  char f2;
};


static class sss: public c, public c2{
public:
  long long m;
} sss;

#define _offsetof(st,f) ((char *)&((st *) 16)->f - (char *) 16)

int main (void) {
  printf ("++Class with longlong inhereting classes with longlong & char:\n");
  printf ("size=%d,align=%d\n", sizeof (sss), __alignof__ (sss));
  printf ("offset-f=%d,offset-f2=%d,offset-m=%d,\nalign-f=%d,align-f2=%d,align-m=%d\n",
          _offsetof (class sss, f), _offsetof (class sss, f2), _offsetof (class sss, m),
          __alignof__ (sss.f), __alignof__ (sss.f2), __alignof__ (sss.m));
  return 0;
}
