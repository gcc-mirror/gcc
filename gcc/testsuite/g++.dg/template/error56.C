// Test that the error message mentions the template arguments.

struct A
{
  template <class T> void f(T);
  void f();
};

int main()
{
  A().f<1>();			// { dg-error "f<1>" }
}
