// { dg-do compile { target c++11 } }
// A C++11 attribute cannot appear in the middle of the decl-specifier-seq,
// only before it (in which case it appertains to the declaration) or at
// the end (in which case it appertains to the type).

struct S {
  friend [[deprecated]] void; // { dg-error "standard attributes in middle of decl-specifiers" }
  friend [[deprecated]] int fn(); // { dg-error "standard attributes in middle of decl-specifiers" }
  // { dg-warning "attribute ignored" "" { target *-*-* } .-1 }
};
