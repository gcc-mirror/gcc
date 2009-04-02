// Testcase for printing typename bindings as well as template args
// in diagnostics (PR c++/25185)

template <class T>
struct A {
  typename T::type f();		// { dg-message "typename T::type = void*" }
  void f(int i = 0);		// { dg-message "" }
};

struct B
{
  typedef void* type;
};

int main()
{
  A<B> a;
  a.f();			// { dg-error "" }
}
