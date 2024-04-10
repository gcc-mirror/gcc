// P0847R7
// { dg-do compile { target c++23 } }
// { dg-options "-pedantic-errors -Wno-volatile" }

// rejecting redeclarations of by-value xobj member functions
// as iobj member functions that are not ref qualified (and vice-versa)
// also check that valid overloads are accepted without diagnostic

//  iobj |  xobj  |  MSVC | clang | ISOC++
//  none |  none  |   X   |   X   |   X
//  none |     c  |   X   |   O   |   X
//  none |     v  |   X   |   O   |   X
//  none |    cv  |   X   |   O   |   X
//     c |  none  |   O   |   O   |   O
//     c |     c  |   O   |   X   |   O
//     c |     v  |   O   |   O   |   O
//     c |    cv  |   O   |   O   |   O
//     v |  none  |   O   |   O   |   O
//     v |     c  |   O   |   O   |   O
//     v |     v  |   O   |   X   |   O
//     v |    cv  |   O   |   O   |   O
//    cv |  none  |   O   |   O   |   O
//    cv |     c  |   O   |   O   |   O
//    cv |     v  |   O   |   O   |   O
//    cv |    cv  |   O   |   X   |   O

/* Top-level cv qualifiers are supposed to be discarded from
   the parameters of a function declaration.
     
   [dcl.fct.5]
   After producing the list of parameter types, any top-level
   cv-qualifiers modifying a parameter type are deleted when forming
   the function type.

   According to the standard, the type of an implicit object parameter
   is always a reference. This isn't reflected in GCC but we still need
   to take this rule into account here.

   [over.match.funcs.general.4]
   For implicit object member functions, the type of the implicit
   object parameter is
   -- “lvalue reference to cv X” for functions declared
      without a ref-qualifier or with the & ref-qualifier
   -- “rvalue reference to cv X” for functions declared with
      the && ref-qualifier
    
   When comparing an iobj and xobj member function to see if they are
   redeclarations we treat them differently depending on if the iobj
   member function has a ref qualifier or not.
   If the iobj member function does not have a ref qualifier, we need to
   strip the top-level references before comparing them.

   [basic.scope.scope.3]
   Two non-static member functions have corresponding object
   parameters if:
   -- exactly one is an implicit object member function with no
      ref-qualifier and the types of their object parameters
      ([dcl.fct]), after removing top-level references, are the
      same, or
   -- their object parameters have the same type. */

struct S {
  void f0(); // { dg-note "previous declaration" }
  void f0(this S); // { dg-error "cannot be overloaded with" }

  void f1(); // { dg-note "previous declaration" }
  void f1(this S const); // { dg-error "cannot be overloaded with" }

  void f2(); // { dg-note "previous declaration" }
  void f2(this S volatile); // { dg-error "cannot be overloaded with" }

  void f3(); // { dg-note "previous declaration" }
  void f3(this S const volatile); // { dg-error "cannot be overloaded with" }

  void fc0() const; // { dg-bogus "" }
  void fc0(this S); // { dg-bogus "" }

  void fc1() const; // { dg-bogus "" }
  void fc1(this S const); // { dg-bogus "" }

  void fc2() const; // { dg-bogus "" }
  void fc2(this S volatile); // { dg-bogus "" }

  void fc3() const; // { dg-bogus "" }
  void fc3(this S const volatile); // { dg-bogus "" }

  void fv0() volatile; // { dg-bogus "" }
  void fv0(this S); // { dg-bogus "" }

  void fv1() volatile; // { dg-bogus "" }
  void fv1(this S const); // { dg-bogus "" }

  void fv2() volatile; // { dg-bogus "" }
  void fv2(this S volatile); // { dg-bogus "" }

  void fv3() volatile; // { dg-bogus "" }
  void fv3(this S const volatile); // { dg-bogus "" }

  void fcv0() const volatile; // { dg-bogus "" }
  void fcv0(this S); // { dg-bogus "" }

  void fcv1() const volatile; // { dg-bogus "" }
  void fcv1(this S const); // { dg-bogus "" }

  void fcv2() const volatile; // { dg-bogus "" }
  void fcv2(this S volatile); // { dg-bogus "" }

  void fcv3() const volatile; // { dg-bogus "" }
  void fcv3(this S const volatile); // { dg-bogus "" }

  // same as the above f cases except reversed

  void g0(this S); // { dg-note "previous declaration" }
  void g0(); // { dg-error "cannot be overloaded with" }

  void g1(this S const); // { dg-note "previous declaration" }
  void g1(); // { dg-error "cannot be overloaded with" }

  void g2(this S volatile); // { dg-note "previous declaration" }
  void g2(); // { dg-error "cannot be overloaded with" }

  void g3(this S const volatile); // { dg-note "previous declaration" }
  void g3(); // { dg-error "cannot be overloaded with" }

  void gc0(this S); // { dg-bogus "" }
  void gc0() const; // { dg-bogus "" }

  void gc1(this S const); // { dg-bogus "" }
  void gc1() const; // { dg-bogus "" }

  void gc2(this S volatile); // { dg-bogus "" }
  void gc2() const; // { dg-bogus "" }

  void gc3(this S const volatile); // { dg-bogus "" }
  void gc3() const; // { dg-bogus "" }

  void gv0(this S); // { dg-bogus "" }
  void gv0() volatile; // { dg-bogus "" }

  void gv1(this S const); // { dg-bogus "" }
  void gv1() volatile; // { dg-bogus "" }

  void gv2(this S volatile); // { dg-bogus "" }
  void gv2() volatile; // { dg-bogus "" }

  void gv3(this S const volatile); // { dg-bogus "" }
  void gv3() volatile; // { dg-bogus "" }

  void gcv0(this S); // { dg-bogus "" }
  void gcv0() const volatile; // { dg-bogus "" }

  void gcv1(this S const); // { dg-bogus "" }
  void gcv1() const volatile; // { dg-bogus "" }

  void gcv2(this S volatile); // { dg-bogus "" }
  void gcv2() const volatile; // { dg-bogus "" }

  void gcv3(this S const volatile); // { dg-bogus "" }
  void gcv3() const volatile; // { dg-bogus "" }
};

