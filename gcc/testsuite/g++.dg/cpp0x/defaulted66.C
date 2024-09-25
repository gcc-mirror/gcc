// PR c++/116162
// { dg-do compile { target c++11 } }
// Check that there is no -Wdefaulted-function-deleted for template
// instantiations.

template<typename>
struct C {
   C();
   C(const C&&) = default; // { dg-error "implicitly deleted" "" { target c++17_down} }
};

struct D {
  D(const D&&) = default; // { dg-error "implicitly deleted" "" { target c++17_down} }
  // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
};

struct M {
  M();
  // So that W wouldn't have had "const W&" copy ctor if it were
  // implicitly declared.
  M(M&);
};

struct W {
   W();
   W(const W&) = default; // { dg-error "implicitly deleted" "" { target c++17_down} }
   // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
   M m;
};

void
g ()
{
  C<int> c;
}
