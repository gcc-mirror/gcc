// { dg-do assemble }


// Based on bug report by Eric NICOLAS <nicolas@bnp-eng.remcomp.com>

namespace foo {
  template<class F> struct bar {};
}

void baz() {
  foo::bar(); // { dg-error "8:cannot deduce template arguments" "" { target c++17 } } template used as expression
  // { dg-error "11:missing template arguments" "" { target c++14_down } .-1 }
}
