// { dg-do compile }
// Properly print CALL_EXPRs while dumping expressions

double g;
int func(double);

template <int>
struct Foo {};

Foo<func(g)> f; // { dg-error "" "func(g)" }
