// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-do compile }

typedef struct
{
  int a;
  enum B { C1, C2, C3 };
  struct T {
    int b;
#if __cplusplus >= 201103L
    static_assert (sizeof (b) == sizeof (int), "");
#endif
    friend int freddy (int);
    friend int garply (int x) { return x; }
  };
  union U { int c; long d; };
  union { int e; long f; };
  int g : 5;
#if __cplusplus >= 201103L
  static_assert (sizeof (a) == sizeof (int), "");
#endif
  friend int qux (int);
  friend int corge (int x) { return x; }
private:
  int h;
protected:
  int i;
public:
  int j;
} S;
struct A {};
typedef struct {					// { dg-message "unnamed class defined here" }
  static int a;						// { dg-error "static data member '<unnamed struct>::a' in unnamed class" }
} B;
typedef struct : public A {				// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  int a;
} C;							// { dg-message "type is not C-compatible because it has a base class" }
#if __cplusplus >= 201103L
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  int b = 42;						// { dg-message "type is not C-compatible because 'D::b' has default member initializer" "" { target c++11 } }
} D;
#endif
struct {						// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  int foo (); } typedef E;				// { dg-message "type is not C-compatible because it contains 'int E::foo\\\(\\\)' declaration" }
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  static int bar ();					// { dg-message "type is not C-compatible because it contains 'static int F::bar\\\(\\\)' declaration" }
} F;
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  typedef int T;					// { dg-message "type is not C-compatible because it contains 'G::T' declaration" }
} G;
#if __cplusplus >= 201103L
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  using T = int;					// { dg-message "type is not C-compatible because it contains 'using H::T = int' declaration" "" { target c++11 } }
} H;
#endif
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  template <int N> struct B { int a; };			// { dg-message "type is not C-compatible because it contains 'template<int N> struct I::B' declaration" }
} I;
typedef struct {					// { dg-message "unnamed class defined here" }
  struct B { static int a; };				// { dg-error "static data member '<unnamed struct>::B::a' in unnamed class" }
} J;
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  struct B : public A { int c; };			// { dg-message "type is not C-compatible because it has a base class" }
} K;
#if __cplusplus >= 201103L
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  struct B { int d = 42; };				// { dg-message "type is not C-compatible because 'L::B::d' has default member initializer" "" { target c++11 } }
} L;
#endif
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  struct B { int foo (); };				// { dg-message "type is not C-compatible because it contains 'int M::B::foo\\\(\\\)' declaration" }
} M;
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  struct B { static int bar (); };			// { dg-message "type is not C-compatible because it contains 'static int N::B::bar\\\(\\\)' declaration" }
} N;
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  struct B { typedef int T; };				// { dg-message "type is not C-compatible because it contains 'O::B::T' declaration" }
} O;
#if __cplusplus >= 201103L
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  struct B { using T = int; };				// { dg-message "type is not C-compatible because it contains 'using P::B::T = int' declaration" "" { target c++11 } }
} P;
#endif
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" }
  struct B { template <int N> struct C { int a; }; };	// { dg-message "type is not C-compatible because it contains 'template<int N> struct Q::B::C' declaration" }
} Q;
#if __cplusplus >= 201103L
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  decltype([](int i){ return i; }) a;			// { dg-message "type is not C-compatible because it contains '\[^\n\r]*R::<lambda\\\(int\\\)>\[^\n\r]*' declaration" "" { target c++11 } }
} R;							// { dg-error "lambda-expression in unevaluated context only available with" "" { target { c++11 && c++17_down } } .-1 }
typedef struct {					// { dg-error "anonymous non-C-compatible type given name for linkage purposes by 'typedef' declaration" "" { target c++11 } }
  struct B { decltype([](int i){ return i; }) a; };	// { dg-message "type is not C-compatible because it contains '\[^\n\r]*T::B::<lambda\\\(int\\\)>\[^\n\r]*' declaration" "" { target c++11 } }
} T;							// { dg-error "lambda-expression in unevaluated context only available with" "" { target { c++11 && c++17_down } } .-1 }
#endif
