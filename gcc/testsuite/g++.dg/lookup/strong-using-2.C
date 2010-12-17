// PR c++/13594

// { dg-options "" }
// { dg-do compile }

namespace foo {
  inline namespace foo_impl {
    class T; // { dg-error "T" "" }
  }
}
namespace bar {
  inline namespace bar_impl {
    class T; // { dg-error "T" "" }
  }
  using namespace foo;
}
namespace baz {
  using namespace foo;
  using namespace bar;
}

foo::T *t1;
bar::T *t2;
baz::T *t3; // { dg-error "(ambiguous|does not name a type)" "" }
