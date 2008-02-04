// Test that with value-initialization, i is initialized to 0
// and the vtable pointer is properly initialized.

// { dg-do run }

struct A
{
  int i;
  virtual void f() {}
};

void f (A& a)
{
  a.f();
}

int main()
{
  A a = A();
  f (a);
  return a.i;
}
