// Test for checking of exception specifications on defaulted fns
// { dg-do compile { target c++11 } }

struct A
{
  A() noexcept = default;
};

A a;

struct B
{
  B() throw (int) = default; // { dg-message "exception-specification" "" { target { ! c++17 } } }
};				// { dg-error "dynamic exception specification" "" { target c++17 } .-1 }
				// { dg-warning "deprecated" "" { target { ! c++17 } } .-2 }
B b;				// { dg-error "deleted" "" { target { ! c++17 } } }

struct C
{
  C() throw (int) { }		// { dg-error "dynamic exception specification" "" { target c++17 } }
};				// { dg-warning "deprecated" "" { target { ! c++17 } } .-1 }

C c;

struct D: C
{
  D() throw (int) = default;	// { dg-error "dynamic exception specification" "" { target c++17 } }
};				// { dg-warning "deprecated" "" { target { ! c++17 } } .-1 }

D d;

struct E
{
  E() = default;
};

E e;
