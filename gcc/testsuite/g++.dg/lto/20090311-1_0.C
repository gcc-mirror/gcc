/* { dg-lto-do run }  */
#include "20090311-1.h"
bool flag;

struct B {
    int a;
    enum { ANOTHER, ONE } f2_;
    float c;
};

extern struct B x[];

struct C {
    int x;
    struct B *p;
    float d;
};

C c = { 0, 0, 3.4 };

struct A {
    enum { UNO, DOS, TRES } f1_;
    int x;
};

A a;

extern int foo();
main()
{
  a.x = 4 + c.x;
  foo();
  return 0;
}
