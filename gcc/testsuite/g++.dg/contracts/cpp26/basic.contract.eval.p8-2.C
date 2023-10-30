// basic.contract.eval/p8
// If a contract violation occurs in a context that is manifestly
// constant-evaluated ([expr.const]), and the evaluation semantic is not
// terminating, then a diagnostic shall be emitted.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }

consteval void foo( auto x ) pre( false ) {}
// { dg-warning {contract predicate is false in constant expression} "" { target *-*-* } .-1 }
int main() {
  foo( 42 );
}
