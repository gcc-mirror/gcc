// { dg-do compile }

// Origin: Paolo Carlini <pcarlini@unitus.it>

// PR c++/5655: Access of member redeclaration.

struct S {
 class A;
 template <class T> class B;
private:
 class A {};			// { dg-error "different access" }
 template <class T> class B {};	// { dg-error "different access" }
};
