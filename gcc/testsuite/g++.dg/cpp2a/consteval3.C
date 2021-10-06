// { dg-do compile { target c++20 } }

struct S { S () : a (0), b (1) {} int a, b; };
int f1 ();		// { dg-message "previous declaration 'int f1\\(\\)'" }
consteval int f1 ();	// { dg-error "redeclaration 'consteval int f1\\(\\)' differs in 'consteval' from previous declaration" }
consteval int f2 ();	// { dg-message "previous declaration 'consteval int f2\\(\\)'" }
int f2 ();		// { dg-error "redeclaration 'int f2\\(\\)' differs in 'consteval' from previous declaration" }
constexpr int f3 ();	// { dg-message "previous declaration 'constexpr int f3\\(\\)'" }
consteval int f3 ();	// { dg-error "redeclaration 'consteval int f3\\(\\)' differs in 'consteval' from previous declaration" }
consteval int f4 ();	// { dg-message "previous declaration 'consteval int f4\\(\\)'" }
constexpr int f4 ();	// { dg-error "redeclaration 'constexpr int f4\\(\\)' differs in 'consteval' from previous declaration" }
typedef consteval int cint;	// { dg-error "'consteval' cannot appear in a typedef declaration" }
consteval struct T { int i; };	// { dg-error "'consteval' cannot be used for type declarations" }
consteval int a = 5;	// { dg-error "a variable cannot be declared 'consteval'" }
consteval auto [ b, c ] = S ();		// { dg-error "structured binding declaration cannot be 'consteval'" }
int f5 (consteval int x) { return x; }	// { dg-error "a parameter cannot be declared 'consteval'" }
consteval int f6 (int x) { return x; }
int d = 6;		// { dg-message "'int d' is not const" }
int e = f6 (d);		// { dg-error "the value of 'd' is not usable in a constant expression" }
constexpr int f7 (int x) { return f6 (x); }	// { dg-error "'x' is not a constant expression" }
constexpr int f = f7 (5);	// { dg-error "" }
				// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1 }
using fnptr = int (int);
fnptr *g = f6;		// { dg-error "taking address of an immediate function 'consteval int f6\\(int\\)'" }
int f8 (fnptr *);
int h = f8 (f6);	// { dg-error "taking address of an immediate function 'consteval int f6\\(int\\)'" }
consteval constexpr int f9 () { return 0; }	// { dg-error "both 'constexpr' and 'consteval' specified" }
constexpr consteval int f10 () { return 0; }	// { dg-error "both 'constexpr' and 'consteval' specified" }
consteval consteval int f11 () { return 0; }	// { dg-error "duplicate 'consteval'" }
struct U { consteval ~U () {} };	// { dg-error "a destructor cannot be 'consteval'" }
struct V { consteval int v = 5; };	// { dg-error "non-static data member 'v' declared 'consteval'" }
struct W { consteval static int w; };	// { dg-error "static data member 'w' declared 'consteval'" }
int i = sizeof (&f6);			// { dg-bogus "taking address of an immediate function 'consteval int f6\\(int\\)'" }
using j = decltype (&f6);		// { dg-bogus "taking address of an immediate function 'consteval int f6\\(int\\)'" }
int k = sizeof (f6 (d));		// { dg-bogus "the value of 'd' is not usable in a constant expression" }
using l = decltype (f6 (d));		// { dg-bogus "the value of 'd' is not usable in a constant expression" }
bool m = noexcept (f6 (d));		// { dg-bogus "the value of 'd' is not usable in a constant expression" }
namespace std {
using size_t = decltype (sizeof (0));
}
consteval void* operator new (std::size_t);	// { dg-error "'operator new' cannot be 'consteval'" }
consteval void operator delete (void *, std::size_t) noexcept;	// { dg-error "'operator delete' cannot be 'consteval'" }
consteval void operator delete[] (void *) noexcept;	// { dg-error "'operator delete \\\[\\\]' cannot be 'consteval'" }
struct X {
  static consteval void* operator new (std::size_t);	// { dg-error "'operator new' cannot be 'consteval'" }
  static consteval void operator delete (void *, std::size_t) noexcept;	// { dg-error "'operator delete' cannot be 'consteval'" }
  consteval static void operator delete[] (void *) noexcept;	// { dg-error "'operator delete \\\[\\\]' cannot be 'consteval'" }
};
consteval int main () { return 0; }	// { dg-error "cannot declare '::main' to be 'consteval'" }
struct A { A (); int a; };		// { dg-message "defaulted constructor calls non-'constexpr' 'A::A\\(\\)'" }
struct B { constexpr B () : b (0) {} int b; };
struct C { A a; consteval C () = default; };	// { dg-error "explicitly defaulted function 'consteval C::C\\(\\)' cannot be declared 'consteval' because the implicit declaration is not 'constexpr'" }
struct D { B b; consteval D () = default; };
template <class T> consteval T f12 (T x) { return x; }
template consteval float f12 (float x); // { dg-error "explicit instantiation shall not use 'consteval' specifier" }
consteval int
f13 (int x)
{
  static int a = 5;		// { dg-error "'a' declared 'static' in 'consteval' function only available with" "" { target c++20_only } }
				// { dg-error "'a' declared 'static' in 'constexpr' context" "" { target c++23 } .-1 }
  thread_local int b = 6;	// { dg-error "'b' declared 'thread_local' in 'consteval' function only available with" "" { target c++20_only } }
  return x;
}
