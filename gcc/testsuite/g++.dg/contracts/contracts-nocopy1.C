// Contracts shouldn't introduce more copies.

// { dg-do compile { target c++20 } }
// { dg-additional-options -fcontracts }

struct A
{
  int i;
  A(int i): i(i) { }
  A(const A&) = delete;
};

A f(A a)
  [[ pre: a.i > 0 ]]
  [[ post r: r.i > 0 ]]
{
  return {a.i};
}

int main()
{
  if (f({42}).i != 42)
    __builtin_abort ();
}
