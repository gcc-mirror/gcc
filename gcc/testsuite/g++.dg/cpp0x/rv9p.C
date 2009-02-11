// PR c++/36744
// { dg-options "-std=c++0x" }
// { dg-do run }

struct S
{
  S(): i(2) {}
  S(S const&s): i(s.i) {}
  int i;
};

void f(S x) { x.i = 0; }

extern "C" void abort (void);
int main()
{
  S y;
  f(static_cast<S&&>(y));
  if (y.i != 2)
    abort ();
  return 0;
}
