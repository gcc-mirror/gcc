// { dg-do assemble  }
struct S
{
  void f(int);
  void f(double);
};

void g(int);
void g(double);

template <int* IP>
void foo();
template <long l>
void foo();

void bar()
{
  foo<S::f>(); // { dg-error "" } no matching function
  foo<g>();    // { dg-error "" } no matching function
  
}
