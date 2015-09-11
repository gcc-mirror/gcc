// PR c++/46736
// { dg-do compile { target c++11 } }

struct U {
  U();
private:
  U(U const&);			// { dg-message "private" }
};

struct X {
  U const u;
  X();
  X(X&&);
};

X::X(X&&)=default;		// { dg-message "implicitly deleted" }
// { dg-prune-output "within this context" }

X f() {
  return X();
}
