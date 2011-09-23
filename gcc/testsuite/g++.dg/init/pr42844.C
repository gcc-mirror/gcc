// PR c++/42844
// { dg-do compile }

struct A
{
  A(){}
};

struct B : A { int i; }; // { dg-message "user-provided default constructor" }

struct C : A { int i; }; // { dg-message "user-provided default constructor" }

struct D : B { D() {} };

struct E { int i; }; // { dg-message "user-provided default constructor" }

template <class T>
struct F : A { T t; }; // { dg-message "user-provided default constructor" }

template <class T>
struct G { T t; }; // { dg-message "user-provided default constructor" }

void f ()
{
  B const b;    // { dg-error "uninitialized const" }
  extern B const bext;

  C const c[ 1 ]; // { dg-error "uninitialized const" }
  extern C const cext[ 1 ];

  D const d;
  extern D const dext;

  E const e;	// { dg-error "uninitialized const" }
  extern E const eext;

  F<int> const f; // { dg-error "uninitialized const" }
  extern F<int> const fext;

  G<int> const g; // { dg-error "uninitialized const" }
  extern G<int> const gext;
}

struct H { int i; }; // { dg-message "user-provided default constructor" }

struct I : A { int i; }; // { dg-message "user-provided default constructor" }

template <class T>
void g ()
{
  T const t; // { dg-error "uninitialized const" }
  extern T const text;
}

template void g<H> ();
template void g<I> ();
