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
  foo<S::f>(); // ERROR - no matching function
  foo<g>();    // ERROR - no matching function
  
}
