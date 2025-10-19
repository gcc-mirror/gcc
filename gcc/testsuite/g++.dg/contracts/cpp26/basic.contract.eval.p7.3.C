// basic.contract.eval/p7.3
// The evaluation of the predicate is performed in a context that is manifestly
// constant-evaluated ([expr.const]) and the predicate is not a core constant
// expression.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts  " }

extern bool runtime_check ();

consteval void foo( auto x ) pre ( runtime_check () ) {}
// { dg-error {contract condition is not constant} "" { target *-*-* } .-1 }
int main() {
  foo( 42 ); // { dg-error {call to consteval function 'foo<int>\(42\)' is not a constant expression} }
}
