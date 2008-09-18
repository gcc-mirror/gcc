// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/11616: Incorrect line number in diagnostics

template <int> struct A
{
  static const int i=0;
};

int baz() { return A<0>::i; }

struct B
{
  static void foo (int);	// { dg-message "candidates" }
};

template <typename T> struct C
{
  virtual void bar() const { T::foo(); } // { dg-error "no matching function" }
};

C<B> c;				// { dg-message "instantiated" }
