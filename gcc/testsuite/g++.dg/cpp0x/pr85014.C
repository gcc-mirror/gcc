// { dg-do compile { target c++11 } }
// { dg-options "" }

struct {
  short a[__builtin_constant_p([] {
    struct {
      int b = b;
      };  // { dg-error "abstract declarator" }
  })];
};  // { dg-error "abstract declarator" }
