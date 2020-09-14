// PR c++/49669

struct Foo { explicit Foo(int) { } };
struct Goo {
  Goo() : x(Foo(4), Foo(5)) { } // { dg-error "" "" { target { ! c++20 } } }
  Foo x[2];
};
