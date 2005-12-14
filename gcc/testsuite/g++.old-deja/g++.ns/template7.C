// { dg-do assemble }


// Based on bug report by Eric NICOLAS <nicolas@bnp-eng.remcomp.com>

namespace foo {
  template<class F> struct bar {};
}

void baz() {
  foo::bar(); // { dg-error "" } template used as expression
}
