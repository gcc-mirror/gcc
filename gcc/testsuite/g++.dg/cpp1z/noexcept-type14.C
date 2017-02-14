// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

#define A asm volatile ("" : : : "memory")
void foo () throw () {}
extern void f1 (decltype (foo) *);	// { dg-bogus "mangled name" }
void f2 (decltype (foo) *);		// { dg-bogus "mangled name" }
extern void f3 (decltype (foo) *);	// { dg-warning "mangled name" "" { target c++14_down } }
void f4 (decltype (foo) *);		// { dg-warning "mangled name" "" { target c++14_down } }
void f5 (decltype (foo) *) { A; }	// { dg-warning "mangled name" "" { target c++14_down } }
static void f6 (decltype (foo) *) { A; }// { dg-bogus "mangled name" }
namespace N {
void f7 (decltype (foo) *) { A; }	// { dg-warning "mangled name" "" { target c++14_down } }
}
namespace {
void f8 (decltype (foo) *) { A; }	// { dg-bogus "mangled name" }
}
void bar ()
{
  f3 (foo);
  f4 (foo);
  f5 (foo);
  f6 (foo);
  N::f7 (foo);
  f8 (foo);
}
