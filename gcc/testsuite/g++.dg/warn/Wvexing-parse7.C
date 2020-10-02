// PR c++/25814
// { dg-do compile }

struct X { };
struct W {
  W(X, X);
};

void
fn ()
{
  W w1(X(), X()); // { dg-warning "parentheses" }
  W w2(X(a), X()); // { dg-warning "parentheses" }
  W w3(X(), X(a)); // { dg-warning "parentheses" }
  W w4(X(a), X(b)); // { dg-warning "parentheses" }
  W w5(X, X);
  W w6(X(a), X);
  W w7(X, X(a));
  W w8(X(a), X()); // { dg-warning "parentheses" }
  W w9(X, X());
  W w10(X, X());

  // Not function declarations.
  W z1(X(), (X()));
  W z2((X()), X());
  W z3((X()), (X()));
}
