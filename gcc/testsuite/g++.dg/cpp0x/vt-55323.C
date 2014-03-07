// { dg-do compile { target c++11 } }

struct foo {
  foo(int a, float b);
};

struct bar : foo {
  template<typename... Args>
  bar(Args&&... args) : foo(2, args){} // { dg-error "parameter packs" }
};

bar b(2,1.);
