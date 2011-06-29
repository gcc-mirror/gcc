// FDIS 5.1.2/8
// { dg-options "-pedantic-errors -std=c++0x" }

struct S2 { void f(int i); };
void S2::f(int i) {
  [&, i]{ };       // OK
  [&, &i]{ };	   // { dg-error "" } i preceded by & when & is the default
  [=, i]{ };       // { dg-error "" } i not preceded by & when = is the default
  [=, this]{ };	   // { dg-error "" } this when = is the default
  [i, i]{ };	   // { dg-error "" } i repeated
  [this, this]{ }; // { dg-error "" } i repeated
}
