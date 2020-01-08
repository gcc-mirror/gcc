// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

struct X { int a, b; };
struct Y { X x; };

void
f()
{
  // This is ok...
  Y y1{{1, 2}};
  Y y2({1, 2});
  // ...but Y y((1,2)) is not the same as Y y{{1,2}}.  (1, 2) is a
  // COMPOUND_EXPR.
  Y y3((1, 2)); // { dg-error "could not convert" }
}
