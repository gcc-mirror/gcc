// PR c++/34395
// { dg-do compile { target c++11 } }

void f(...);
template<int... N> void foo (int... x[N])	// { dg-message "declared here" }
{
  struct A
  {
    A () { f(x...); }		// { dg-error "use of parameter from containing function" }
  };
}

int main()
{
  int ar[4];
  foo<4>(ar);
}
