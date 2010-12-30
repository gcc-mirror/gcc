// PR c++/46736
// { dg-options -std=c++0x }

struct U {
  U();
  U(U const&);
};

struct X {
  U const u;
  X();
  X(X&&);
};

X::X(X&&)=default;		// { dg-error "implicitly deleted" }
// { dg-error "does not have a move constructor" "" { target *-*-* } 15 }

X f() {
  return X();
}
