// { dg-do compile }
// { dg-options "-fopenmp" }

struct S { int s; S () : s (0) {} S (int x) : s (x) {} ~S () {} };
struct T { int t; T () : t (0) {} T (int x) : t (x) {} ~T () {} };

#pragma omp declare reduction (+: ::S: omp_out.s += omp_in.s)
#pragma omp declare reduction (*: S: omp_out.s *= omp_in.s) \
		    initializer (omp_priv (1))
#pragma omp declare reduction (foo: S: omp_out.s += omp_in.s)

void
f1 ()
{
  S s, s2;
  T t;
  #pragma omp declare reduction (+: T: omp_out.t += omp_in.t)
  #pragma omp parallel reduction (+: t) reduction (foo: s) reduction (*: s2)
  s.s = 1, t.t = 1, s2.s = 2;
  #pragma omp parallel reduction (::operator +: s)
  s.s = 1;
  #pragma omp parallel reduction (+: s)
  s.s = 1;
}

template <int N>
int
f2 ()
{
  S s, s2;
  T t;
  #pragma omp declare reduction (+: T: omp_out.t += omp_in.t)
  #pragma omp parallel reduction (+: t) reduction (foo: s) reduction (*: s2)
  s.s = 1, t.t = 1, s2.s = 2;
  #pragma omp parallel reduction (::operator +: s)
  s.s = 1;
  #pragma omp parallel reduction (+: s)
  s.s = 1;
  return 0;
}

int x = f2<0> ();

void bar (S &);

void
f3 ()
{
  #pragma omp declare reduction (foo: S: omp_out.s += omp_in.s) initializer (bar (omp_priv))
  #pragma omp declare reduction (bar: S: omp_out.s += omp_in.s) initializer (bar (omp_orig)) // { dg-error "one of the initializer call arguments should be" }
}

template <typename T>
int
f4 ()
{
  #pragma omp declare reduction (foo: T: omp_out.s += omp_in.s) initializer (bar (omp_priv))
  #pragma omp declare reduction (bar: T: omp_out.s += omp_in.s) initializer (bar (omp_orig)) // { dg-error "one of the initializer call arguments should be" }
  return 0;
}

int y = f4 <S> ();

namespace N1
{
  #pragma omp declare reduction (+: ::S: omp_out.s *= omp_in.s)		// { dg-error "previous" }
  #pragma omp declare reduction (+: S: omp_out.s += omp_in.s)		// { dg-error "redeclaration of" }
  void
  f5 ()
  {
    #pragma omp declare reduction (f5: S: omp_out.s *= omp_in.s)	// { dg-error "previous" }
    #pragma omp declare reduction (f5: ::S: omp_out.s += omp_in.s)	// { dg-error "redeclaration of" }
  }
}

namespace N2
{
  struct U
  {
    #pragma omp declare reduction (bar: S: omp_out.s *= omp_in.s)	// { dg-error "with" }
    #pragma omp declare reduction (bar: S: omp_out.s += omp_in.s)	// { dg-error "cannot be overloaded" }
  };
}

namespace N3
{
  #pragma omp declare reduction (+: ::S: omp_out.s *= omp_in.s)		// { dg-error "previous" }
  #pragma omp declare reduction (+: T: omp_out.t += omp_in.t)
  #pragma omp declare reduction (+: S: omp_out.s += omp_in.s)		// { dg-error "redeclaration of" }
  #pragma omp declare reduction (n3: long: omp_out += omp_in)		// { dg-error "previous" }
  #pragma omp declare reduction (n3: long int: omp_out += omp_in)	// { dg-error "redeclaration of" }
  #pragma omp declare reduction (n3: short unsigned: omp_out += omp_in)
  #pragma omp declare reduction (n3: short int: omp_out += omp_in)
  void
  f6 ()
  {
    #pragma omp declare reduction (f6: T: omp_out.t += omp_in.t)
    #pragma omp declare reduction (f6: S: omp_out.s *= omp_in.s)	// { dg-error "previous" }
    #pragma omp declare reduction (f6: ::S: omp_out.s += omp_in.s)	// { dg-error "redeclaration of" }
    #pragma omp declare reduction (f6: long: omp_out += omp_in)		// { dg-error "previous" }
    #pragma omp declare reduction (f6: long int: omp_out += omp_in)	// { dg-error "redeclaration of" }
    #pragma omp declare reduction (f6: short unsigned: omp_out += omp_in)
    #pragma omp declare reduction (f6: short int: omp_out += omp_in)
  }
}

namespace N4
{
  struct U
  {
    #pragma omp declare reduction (bar: T: omp_out.t += omp_in.t)
    #pragma omp declare reduction (bar: S: omp_out.s *= omp_in.s)	// { dg-error "with" }
    #pragma omp declare reduction (bar: S: omp_out.s += omp_in.s)	// { dg-error "cannot be overloaded" }
    #pragma omp declare reduction (bar: long: omp_out += omp_in)	// { dg-error "with" }
    #pragma omp declare reduction (bar: long int: omp_out += omp_in)	// { dg-error "cannot be overloaded" }
    #pragma omp declare reduction (bar: short unsigned: omp_out += omp_in)
    #pragma omp declare reduction (bar: short int: omp_out += omp_in)
  };
}

namespace N5
{
  template <typename T>
  int
  f7 ()
  {
    #pragma omp declare reduction (f7: T: omp_out.s *= omp_in.s)	// { dg-error "previous" }
    #pragma omp declare reduction (f7: T: omp_out.s += omp_in.s)	// { dg-error "redeclaration of" }
    return 0;
  }
  int x = f7 <S> ();
  template <typename T>
  struct U
  {
    #pragma omp declare reduction (bar: T: omp_out.s *= omp_in.s)	// { dg-error "with" }
    #pragma omp declare reduction (bar: T: omp_out.s += omp_in.s)	// { dg-error "cannot be overloaded" }
  };
  U<S> u;
}

namespace N6
{
  template <typename U>
  int
  f8 ()
  {
    #pragma omp declare reduction (f8: T: omp_out.t += omp_in.t)
    #pragma omp declare reduction (f8: U: omp_out.s *= omp_in.s)	// { dg-error "previous" }
    #pragma omp declare reduction (f8: ::S: omp_out.s += omp_in.s)	// { dg-error "redeclaration of" }
    #pragma omp declare reduction (f8: long: omp_out += omp_in)		// { dg-error "previous" }
    #pragma omp declare reduction (f8: long int: omp_out += omp_in)	// { dg-error "redeclaration of" }
    #pragma omp declare reduction (f8: short unsigned: omp_out += omp_in)
    #pragma omp declare reduction (f8: short int: omp_out += omp_in)
    return 0;
  }
  int x = f8 <S> ();
  template <typename V>
  struct U
  {
    typedef V V2;
    #pragma omp declare reduction (bar: T: omp_out.t += omp_in.t)
    #pragma omp declare reduction (bar: V: omp_out.s *= omp_in.s)	// { dg-error "with" }
    #pragma omp declare reduction (bar: V2: omp_out.s += omp_in.s)	// { dg-error "cannot be overloaded" }
    #pragma omp declare reduction (bar: long: omp_out += omp_in)	// { dg-error "with" }
    #pragma omp declare reduction (bar: long int: omp_out += omp_in)	// { dg-error "cannot be overloaded" }
    #pragma omp declare reduction (bar: short unsigned: omp_out += omp_in)
    #pragma omp declare reduction (bar: short int: omp_out += omp_in)
  };
  U<S> u;
}

namespace N7
{
  #pragma omp declare reduction (+: S: omp_out.s += omp_in.s) initializer (omp_priv) // { dg-error "invalid initializer clause" }
  #pragma omp declare reduction (+: T: omp_out.t += omp_in.t) initializer (omp_priv ()) // { dg-error "invalid initializer clause" }
}

namespace N8
{
  struct A { int a; A (); ~A (); };
  struct B { int b; B (); ~B (); B (int); };
  struct C : public A, B { int c; C (); ~C (); };
  #pragma omp declare reduction (+:B:omp_out.b += omp_in.b) initializer (omp_priv (4))
  void bar (C &);
  void baz ()
  {
    C a;
    #pragma omp parallel reduction (+:a) // { dg-error "user defined reduction with constructor initializer for base class" }
    bar (a);
  }
}
