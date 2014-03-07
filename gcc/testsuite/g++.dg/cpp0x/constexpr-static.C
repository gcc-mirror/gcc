// Test for constant initialization of non-literal class (e.g. mutex)
// { dg-options "-save-temps" }
// { dg-do run { target c++11 } }

struct A
{
  int i;
  constexpr A(int _i): i(_i) { }
  A(const A&);		       // non-trivial copy ctor makes A non-literal
};

A a(42);	    // constexpr constructor allows constant initialization
A ar[3] = { { 1 }, { 2 }, { 3 } };
// { dg-final { scan-assembler-not "static_initialization" } }
// { dg-final cleanup-saved-temps }

int main()
{
  if (a.i != 42
      || ar[0].i != 1
      || ar[1].i != 2
      || ar[2].i != 3)
    return 1;
}
