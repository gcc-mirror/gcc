// Test for noexcept-specification
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X, #X)

void f();
void f() noexcept(false);
void f() noexcept(1 == 0);
void f();

SA(!noexcept(f()));

void g() throw (int);		// { dg-message "previous declaration" "" { target { ! c++1z } } }
				// { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
				// { dg-warning "deprecated" "" { target { ! c++1z } } .-2 }
void g() noexcept(false);	// { dg-error "different exception" "" { target { ! c++1z } } }
void g();

void h() throw();
void h() noexcept;
void h() throw();
void h() noexcept;

template <class T>
void g (T) noexcept(noexcept(T())); // { dg-message "previous declaration" }
template <class T>
void g (T) noexcept(noexcept(T(0))); // { dg-error "different exception" }

template <class T>
void f (T) noexcept(noexcept(T()) && noexcept(T()));
template <class T>
void f (T) noexcept(noexcept(T()) && noexcept(T()));
template <class T>
void f2(T a) noexcept (noexcept (f (a)));

struct A { A(); };
SA(noexcept(f(1)));
SA(!noexcept(f(A())));
SA(noexcept(f2(1)));
SA(!noexcept(f2(A())));

template <class... Ts>
void f3(Ts... ts) noexcept (noexcept (f(ts...)));

SA(noexcept(f3(1)));
SA(!noexcept(f3(A())));

template <class T1, class T2>
void f (T1, T2) noexcept(noexcept(T1(), T2()));

struct B { };

SA(noexcept(f3(1,B())));
SA(!noexcept(f3(1,A())));
SA(!noexcept(f3(A(),1)));
SA(!noexcept(f3(A(),A())));
