// { dg-do compile { target c++11 } }
// PR c++/84733 ICE popping local binding after cleanup region

struct c {
  ~c();
} b;

void f() {
#ifndef OK
  try {
  d:
    ;
  } catch (int) {
  }
#endif
  decltype(b) a;
  int e;
  struct e { } f;
  e = 5;
  struct e j;
}
