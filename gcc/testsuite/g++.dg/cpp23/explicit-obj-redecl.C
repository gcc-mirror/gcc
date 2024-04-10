// P0847R7
// { dg-do compile { target c++23 } }

// redeclarations of xobj/iobj member functions where the iobj member function
// is not ref qualified

// it does not make sense to check for the inverse in this test (7 iobj, 1 xobj)
// because you are not allowed to overload iobj member functions without ref qualifiers
// with iobj member functions that do (and vice versa)

// iobj first

struct S0 {
  void f0(); // { dg-note "previous declaration" }
  void f0(this S0 &&); // { dg-error "cannot be overloaded with" }
  void f0(this S0 const&); // { dg-bogus "" }
  void f0(this S0 const&&); // { dg-bogus "" }
  void f0(this S0 volatile&); // { dg-bogus "" }
  void f0(this S0 volatile&&); // { dg-bogus "" }
  void f0(this S0 const volatile&); // { dg-bogus "" }
  void f0(this S0 const volatile&&); // { dg-bogus "" }

  void f1(); // { dg-note "previous declaration" }
  void f1(this S0 &); // { dg-error "cannot be overloaded with" }
  void f1(this S0 const&); // { dg-bogus "" }
  void f1(this S0 const&&); // { dg-bogus "" }
  void f1(this S0 volatile&); // { dg-bogus "" }
  void f1(this S0 volatile&&); // { dg-bogus "" }
  void f1(this S0 const volatile&); // { dg-bogus "" }
  void f1(this S0 const volatile&&); // { dg-bogus "" }

  void fc0() const; // { dg-note "previous declaration" }
  void fc0(this S0 &); // { dg-bogus "" }
  void fc0(this S0 &&); // { dg-bogus "" }
  void fc0(this S0 const&&); // { dg-error "cannot be overloaded with" }
  void fc0(this S0 volatile&); // { dg-bogus "" }
  void fc0(this S0 volatile&&); // { dg-bogus "" }
  void fc0(this S0 const volatile&); // { dg-bogus "" }
  void fc0(this S0 const volatile&&); // { dg-bogus "" }

  void fc1() const; // { dg-note "previous declaration" }
  void fc1(this S0 &); // { dg-bogus "" }
  void fc1(this S0 &&); // { dg-bogus "" }
  void fc1(this S0 const&); // { dg-error "cannot be overloaded with" }
  void fc1(this S0 volatile&); // { dg-bogus "" }
  void fc1(this S0 volatile&&); // { dg-bogus "" }
  void fc1(this S0 const volatile&); // { dg-bogus "" }
  void fc1(this S0 const volatile&&); // { dg-bogus "" }

  void fv0() volatile; // { dg-note "previous declaration" }
  void fv0(this S0 &); // { dg-bogus "" }
  void fv0(this S0 &&); // { dg-bogus "" }
  void fv0(this S0 const&); // { dg-bogus "" }
  void fv0(this S0 const&&); // { dg-bogus "" }
  void fv0(this S0 volatile&&); // { dg-error "cannot be overloaded with" }
  void fv0(this S0 const volatile&); // { dg-bogus "" }
  void fv0(this S0 const volatile&&); // { dg-bogus "" }

  void fv1() volatile; // { dg-note "previous declaration" }
  void fv1(this S0 &); // { dg-bogus "" }
  void fv1(this S0 &&); // { dg-bogus "" }
  void fv1(this S0 const&); // { dg-bogus "" }
  void fv1(this S0 const&&); // { dg-bogus "" }
  void fv1(this S0 volatile&); // { dg-error "cannot be overloaded with" }
  void fv1(this S0 const volatile&); // { dg-bogus "" }
  void fv1(this S0 const volatile&&); // { dg-bogus "" }

  void fcv0() const volatile; // { dg-note "previous declaration" }
  void fcv0(this S0 &); // { dg-bogus "" }
  void fcv0(this S0 &&); // { dg-bogus "" }
  void fcv0(this S0 const&); // { dg-bogus "" }
  void fcv0(this S0 const&&); // { dg-bogus "" }
  void fcv0(this S0 volatile&); // { dg-bogus "" }
  void fcv0(this S0 volatile&&); // { dg-bogus "" }
  void fcv0(this S0 const volatile&&); // { dg-error "cannot be overloaded with" }

  void fcv1() const volatile; // { dg-note "previous declaration" }
  void fcv1(this S0 &); // { dg-bogus "" }
  void fcv1(this S0 &&); // { dg-bogus "" }
  void fcv1(this S0 const&); // { dg-bogus "" }
  void fcv1(this S0 const&&); // { dg-bogus "" }
  void fcv1(this S0 volatile&); // { dg-bogus "" }
  void fcv1(this S0 volatile&&); // { dg-bogus "" }
  void fcv1(this S0 const volatile&); // { dg-error "cannot be overloaded with" }
};

// iobj last

struct S1 {
  void f0(this S1 &&); // { dg-note "previous declaration" }
  void f0(this S1 const&); // { dg-bogus "" }
  void f0(this S1 const&&); // { dg-bogus "" }
  void f0(this S1 volatile&); // { dg-bogus "" }
  void f0(this S1 volatile&&); // { dg-bogus "" }
  void f0(this S1 const volatile&); // { dg-bogus "" }
  void f0(this S1 const volatile&&); // { dg-bogus "" }
  void f0(); // { dg-error "cannot be overloaded with" }

  void f1(this S1 &); // { dg-note "previous declaration" }
  void f1(this S1 const&); // { dg-bogus "" }
  void f1(this S1 const&&); // { dg-bogus "" }
  void f1(this S1 volatile&); // { dg-bogus "" }
  void f1(this S1 volatile&&); // { dg-bogus "" }
  void f1(this S1 const volatile&); // { dg-bogus "" }
  void f1(this S1 const volatile&&); // { dg-bogus "" }
  void f1(); // { dg-error "cannot be overloaded with" }

  void fc0(this S1 &); // { dg-bogus "" }
  void fc0(this S1 &&); // { dg-bogus "" }
  void fc0(this S1 const&&); // { dg-note "previous declaration" }
  void fc0(this S1 volatile&); // { dg-bogus "" }
  void fc0(this S1 volatile&&); // { dg-bogus "" }
  void fc0(this S1 const volatile&); // { dg-bogus "" }
  void fc0(this S1 const volatile&&); // { dg-bogus "" }
  void fc0() const; // { dg-error "cannot be overloaded with" }

  void fc1(this S1 &); // { dg-bogus "" }
  void fc1(this S1 &&); // { dg-bogus "" }
  void fc1(this S1 const&); // { dg-note "previous declaration" }
  void fc1(this S1 volatile&); // { dg-bogus "" }
  void fc1(this S1 volatile&&); // { dg-bogus "" }
  void fc1(this S1 const volatile&); // { dg-bogus "" }
  void fc1(this S1 const volatile&&); // { dg-bogus "" }
  void fc1() const; // { dg-error "cannot be overloaded with" }

  void fv0(this S1 &); // { dg-bogus "" }
  void fv0(this S1 &&); // { dg-bogus "" }
  void fv0(this S1 const&); // { dg-bogus "" }
  void fv0(this S1 const&&); // { dg-bogus "" }
  void fv0(this S1 volatile&&); // { dg-note "previous declaration" }
  void fv0(this S1 const volatile&); // { dg-bogus "" }
  void fv0(this S1 const volatile&&); // { dg-bogus "" }
  void fv0() volatile; // { dg-error "cannot be overloaded with" }

  void fv1(this S1 &); // { dg-bogus "" }
  void fv1(this S1 &&); // { dg-bogus "" }
  void fv1(this S1 const&); // { dg-bogus "" }
  void fv1(this S1 const&&); // { dg-bogus "" }
  void fv1(this S1 volatile&); // { dg-note "previous declaration" }
  void fv1(this S1 const volatile&); // { dg-bogus "" }
  void fv1(this S1 const volatile&&); // { dg-bogus "" }
  void fv1() volatile; // { dg-error "cannot be overloaded with" }

  void fcv0(this S1 &); // { dg-bogus "" }
  void fcv0(this S1 &&); // { dg-bogus "" }
  void fcv0(this S1 const&); // { dg-bogus "" }
  void fcv0(this S1 const&&); // { dg-bogus "" }
  void fcv0(this S1 volatile&); // { dg-bogus "" }
  void fcv0(this S1 volatile&&); // { dg-bogus "" }
  void fcv0(this S1 const volatile&&); // { dg-note "previous declaration" }
  void fcv0() const volatile; // { dg-error "cannot be overloaded with" }

  void fcv1(this S1 &); // { dg-bogus "" }
  void fcv1(this S1 &&); // { dg-bogus "" }
  void fcv1(this S1 const&); // { dg-bogus "" }
  void fcv1(this S1 const&&); // { dg-bogus "" }
  void fcv1(this S1 volatile&); // { dg-bogus "" }
  void fcv1(this S1 volatile&&); // { dg-bogus "" }
  void fcv1(this S1 const volatile&); // { dg-note "previous declaration" }
  void fcv1() const volatile; // { dg-error "cannot be overloaded with" }
};

// in order (iobj replacing one of the following in each group)
// lvalue ref to S
// rvalue ref to S
// lvalue c ref to S
// rvalue c ref to S
// lvalue v ref to S
// rvalue v ref to S
// lvalue cv ref to S
// rvalue cv ref to S

struct S2 {
  void f0(); // { dg-note "previous declaration" }
  void f0(this S2 &&); // { dg-error "cannot be overloaded with" }
  void f0(this S2 const&); // { dg-bogus "" }
  void f0(this S2 const&&); // { dg-bogus "" }
  void f0(this S2 volatile&); // { dg-bogus "" }
  void f0(this S2 volatile&&); // { dg-bogus "" }
  void f0(this S2 const volatile&); // { dg-bogus "" }
  void f0(this S2 const volatile&&); // { dg-bogus "" }

  void f1(this S2 &); // { dg-note "previous declaration" }
  void f1(); // { dg-error "cannot be overloaded with" }
  void f1(this S2 const&); // { dg-bogus "" }
  void f1(this S2 const&&); // { dg-bogus "" }
  void f1(this S2 volatile&); // { dg-bogus "" }
  void f1(this S2 volatile&&); // { dg-bogus "" }
  void f1(this S2 const volatile&); // { dg-bogus "" }
  void f1(this S2 const volatile&&); // { dg-bogus "" }

  void fc0(this S2 &); // { dg-bogus "" }
  void fc0(this S2 &&); // { dg-bogus "" }
  void fc0() const; // { dg-note "previous declaration" }
  void fc0(this S2 const&&); // { dg-error "cannot be overloaded with" }
  void fc0(this S2 volatile&); // { dg-bogus "" }
  void fc0(this S2 volatile&&); // { dg-bogus "" }
  void fc0(this S2 const volatile&); // { dg-bogus "" }
  void fc0(this S2 const volatile&&); // { dg-bogus "" }

  void fc1(this S2 &); // { dg-bogus "" }
  void fc1(this S2 &&); // { dg-bogus "" }
  void fc1(this S2 const&); // { dg-note "previous declaration" }
  void fc1() const; // { dg-error "cannot be overloaded with" }
  void fc1(this S2 volatile&); // { dg-bogus "" }
  void fc1(this S2 volatile&&); // { dg-bogus "" }
  void fc1(this S2 const volatile&); // { dg-bogus "" }
  void fc1(this S2 const volatile&&); // { dg-bogus "" }

  void fv0(this S2 &); // { dg-bogus "" }
  void fv0(this S2 &&); // { dg-bogus "" }
  void fv0(this S2 const&); // { dg-bogus "" }
  void fv0(this S2 const&&); // { dg-bogus "" }
  void fv0() volatile; // { dg-note "previous declaration" }
  void fv0(this S2 volatile&&); // { dg-error "cannot be overloaded with" }
  void fv0(this S2 const volatile&); // { dg-bogus "" }
  void fv0(this S2 const volatile&&); // { dg-bogus "" }

  void fv1(this S2 &); // { dg-bogus "" }
  void fv1(this S2 &&); // { dg-bogus "" }
  void fv1(this S2 const&); // { dg-bogus "" }
  void fv1(this S2 const&&); // { dg-bogus "" }
  void fv1(this S2 volatile&); // { dg-note "previous declaration" }
  void fv1() volatile; // { dg-error "cannot be overloaded with" }
  void fv1(this S2 const volatile&); // { dg-bogus "" }
  void fv1(this S2 const volatile&&); // { dg-bogus "" }

  void fcv0(this S2 &); // { dg-bogus "" }
  void fcv0(this S2 &&); // { dg-bogus "" }
  void fcv0(this S2 const&); // { dg-bogus "" }
  void fcv0(this S2 const&&); // { dg-bogus "" }
  void fcv0(this S2 volatile&); // { dg-bogus "" }
  void fcv0(this S2 volatile&&); // { dg-bogus "" }
  void fcv0() const volatile; // { dg-note "previous declaration" }
  void fcv0(this S2 const volatile&&); // { dg-error "cannot be overloaded with" }

  void fcv1(this S2 &); // { dg-bogus "" }
  void fcv1(this S2 &&); // { dg-bogus "" }
  void fcv1(this S2 const&); // { dg-bogus "" }
  void fcv1(this S2 const&&); // { dg-bogus "" }
  void fcv1(this S2 volatile&); // { dg-bogus "" }
  void fcv1(this S2 volatile&&); // { dg-bogus "" }
  void fcv1(this S2 const volatile&); // { dg-note "previous declaration" }
  void fcv1() const volatile; // { dg-error "cannot be overloaded with" }
};

