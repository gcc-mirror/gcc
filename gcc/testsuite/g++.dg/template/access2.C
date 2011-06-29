// { dg-do compile }

// PR c++/5387
// Enforcing access of typename type.

template <class T> struct A {
  typename T::X x;			// { dg-error "this context" }
  int f() { return T::i; }		// { dg-error "this context" }
};

class B {
  typedef int X;			// { dg-error "private" }
  static int i;				// { dg-error "private" }
};

int main()
{
  A<B> ab;				// { dg-message "required" }
  ab.f();				// { dg-message "required" }
}
