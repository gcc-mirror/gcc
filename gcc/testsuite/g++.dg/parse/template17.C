// PR c++/23841

template <int I>
struct S
{
  int f(int i = I) { return i; }
};

void
g ()
{
  S<(int)0.> a2;
}
