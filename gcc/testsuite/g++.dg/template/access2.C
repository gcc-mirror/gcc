// { dg-do compile }

// PR c++/5387
// Enforcing access of typename type.

template <class T> struct A {
  typename T::X x;			// { dg-error "this context" }
};

template <class T> struct A2 {
  int f() { return T::i; }		// { dg-error "this context" }
};

class B {
  typedef int X;			// { dg-message "private" }
  static int i;				// { dg-message "private" }
};

int main()
{
  A<B> ab;				// { dg-message "required" }
  A2<B> a2b;
  a2b.f();				// { dg-message "required" }
}
