// PR c++/19628
// Verify that __builtin_constant_p may appear in a constant-expression.

// { dg-do compile }

template <int I>
int f(int x[__builtin_constant_p(I)])
{
  return x[0];
}

int g()
{
  int a[1] = { 7 };
  return f<32>(a);
}
