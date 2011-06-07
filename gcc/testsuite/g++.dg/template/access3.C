// { dg-do compile }

// PR c++/5387
// Enforcing access of typename type.

template <class T> struct A {
  typename T::template X<int> x;	// { dg-error "this context" }
};

class B {
  template <class T> class X {};	// { dg-error "private" }
};

int main()
{
  A<B> ab;				// { dg-message "required" }
}
