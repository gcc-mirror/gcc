// PR c++/12696

struct A {
  static float b[10];
} // { dg-error "after struct definition" }

float A::b[] = {1,2,3};
