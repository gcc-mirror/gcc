// PR c++/13594

// { dg-do compile }

namespace foo_impl {
  class T; // { dg-error "T" "" }
}
namespace bar_impl {
  class T; // { dg-error "T" "" }
}
namespace foo {
  using namespace foo_impl __attribute__((strong));
}
namespace bar {
  using namespace bar_impl __attribute__((strong));
  using namespace foo;
}
namespace baz {
  using namespace foo;
  using namespace bar;
}

foo::T *t1;
bar::T *t2;
baz::T *t3; // { dg-error "(ambiguous|expected|extra)" "" }
