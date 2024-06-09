// { dg-do compile { target c++14 } }

union U {
  int a;
  float b;
};

constexpr bool foo() {
  U u {};
  u.b = 1.0f;  // { dg-error "change of the active member" "" { target c++17_down } }
  return u.b == 1.0f;
}
constexpr bool x = foo();
