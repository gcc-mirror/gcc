// PR c++/12696

struct A {
  static float b[10];
}

float A::b[] = {1,2,3}; // { dg-error "" }
