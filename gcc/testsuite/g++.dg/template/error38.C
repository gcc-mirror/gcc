// Testcase for printing typename/typedef bindings as well as template args
// in diagnostics (PR c++/25185)

template <class T>
struct A {
  typename T::type f();		// { dg-message "typename T::type = void*" }
  void f(int i = 0);		// { dg-message "" }

  typedef typename T::type mytype;
  mytype g();			// { dg-message "mytype = void*" }
  void g(int i = 0);		// { dg-message "" }
};

struct B
{
  typedef void* type;
};

// Also make sure that deduced template arguments get canonicalized.

template <class T>
void f (T &t);			// { dg-message "T = int" }

template <class T>
void f (T &t, int = 0);		// { dg-message "" }

typedef int myint;
myint i;
myint *p;

int main()
{
  A<B> a;
  a.f();			// { dg-error "" }
  a.g();			// { dg-error "" }

  f(i);				// { dg-error "" }
  f(p);				// { dg-error "" }
}
