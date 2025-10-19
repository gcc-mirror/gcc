// basic.contract.eval/p8
// If a contract violation occurs in a context that is manifestly
// constant-evaluated ([expr.const]), and the evaluation semantic is a
// terminating semantic, the program is ill-formed.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts  " }

consteval void foo( auto x ) pre( false ) {}
// { dg-error {contract predicate is false in constant expression} "" { target *-*-* } .-1 }
int main() {
  foo( 42 ); // { dg-error {call to consteval function 'foo<int>\(42\)' is not a constant expression} }
}
