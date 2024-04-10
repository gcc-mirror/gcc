// P0847R7
// { dg-do compile { target c++23 } }

// valid overloading of iobj member functions without ref qualifiers
// with xobj member functions (and vice-versa)

// this is the most you can mix these, it may look short but it does test
// all allowed cases (other than by-value and unrelated types)

// [over.match.funcs.general.4]
// For implicit object member functions, the type of the implicit
// object parameter is
// -- “lvalue reference to cv X” for functions declared
//    without a ref-qualifier or with the & ref-qualifier
// -- “rvalue reference to cv X” for functions declared with
//    the && ref-qualifier

// [basic.scope.scope.3]
// Two non-static member functions have corresponding object
// parameters if:
// -- exactly one is an implicit object member function with no
//    ref-qualifier and the types of their object parameters
//    ([dcl.fct]), after removing top-level references, are the
//    same, or

// in simpler terms, only the cv qualification of the explicit/implicit object
// parameter matter for determining whether these are redeclarations or overloads
// (when a ref qualifier is not present on the iobj member function)

// xobj first, iobj last

struct S0 {
  void f(this S0 &);
  void f(this S0 &&);
  void f() const;
  void f() volatile;
  void f() const volatile;

  void fc(this S0 const&);
  void fc(this S0 const&&);
  void fc();
  void fc() volatile;
  void fc() const volatile;

  void fv(this S0 volatile&);
  void fv(this S0 volatile&&);
  void fv();
  void fv() const;
  void fv() const volatile;

  void fcv(this S0 const volatile&);
  void fcv(this S0 const volatile&&);
  void fcv();
  void fcv() const;
  void fcv() volatile;
};

// iobj first, xobj last

struct S1 {
  void f() const;
  void f() volatile;
  void f() const volatile;
  void f(this S1 &);
  void f(this S1 &&);

  void fc();
  void fc() volatile;
  void fc() const volatile;
  void fc(this S1 const&);
  void fc(this S1 const&&);

  void fv();
  void fv() const;
  void fv() const volatile;
  void fv(this S1 volatile&);
  void fv(this S1 volatile&&);

  void fcv();
  void fcv() const;
  void fcv() volatile;
  void fcv(this S1 const volatile&);
  void fcv(this S1 const volatile&&);
};

// in order

struct S2 {
  void f(this S2 &);
  void f(this S2 &&);
  void f() const;
  void f() volatile;
  void f() const volatile;

  void fc();
  void fc(this S2 const&);
  void fc(this S2 const&&);
  void fc() volatile;
  void fc() const volatile;

  void fv();
  void fv() const;
  void fv(this S2 volatile&);
  void fv(this S2 volatile&&);
  void fv() const volatile;

  void fcv();
  void fcv() const;
  void fcv() volatile;
  void fcv(this S2 const volatile&);
  void fcv(this S2 const volatile&&);
};

