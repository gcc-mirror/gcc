// Build don't link:

// crash test - XFAIL *-*-*

// Based on bug report by Eric NICOLAS <nicolas@bnp-eng.remcomp.com>

namespace foo {
  template<class F> struct bar {};
}

void baz() {
  foo::bar(); // ERROR - template used as expression
}
