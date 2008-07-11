// { dg-do compile }
// { dg-options "-fshow-column" }
// Properly print CALL_EXPRs while dumping expressions

double g;
int func(double);

template <int>
struct Foo {};

Foo<func(g)> f; // { dg-error "5: error: 'int func.double.' cannot appear in a constant-expression" "" { target *-*-* } { 11 } }
// { dg-error "10: error: 'g' cannot appear in a constant-expression" "" { target *-*-* } { 11 } }
// { dg-error "11: error: a function call cannot appear in a constant-expression" "" { target *-*-* } { 11 } }
// { dg-error "12: error: template argument 1 is invalid" "" { target *-*-* } { 11 } }
// { dg-error "15: error: invalid type in declaration before ';' token" "" { target *-*-* } { 11 } }
