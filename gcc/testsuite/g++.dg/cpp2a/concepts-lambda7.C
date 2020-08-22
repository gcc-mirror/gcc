// PR c++/95020
// { dg-do compile { target c++2a } }

template<typename>
void foo() {
  auto t = [](auto v) {
    static_assert(requires { *v; }); // { dg-error "static assertion failed" }
  };
  t(0);
}

void bar() {
  foo<void>();
}
