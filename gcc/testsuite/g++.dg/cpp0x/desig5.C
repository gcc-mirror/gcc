// PR c++/87324
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }

struct {
  struct {
    double a;
    struct {
      short b;
    };
  };
  int c;
} d{.a = 7, .a = 8.09};  // { dg-error "designator used multiple times in the same initializer list" }
