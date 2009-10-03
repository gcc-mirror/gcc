#include "20090311-1.h"

struct A {
    enum { UNO, DOS, TRES } f1_;
    int x;
};

struct B;

extern struct B x[];

struct C {
    int x;
    struct B *p;
    float d;
};

extern A a;
extern B b;
extern bool flag;
extern C c;

int foo()
{
  if (!flag)
    return a.x - c.x;
  return 0;
}
