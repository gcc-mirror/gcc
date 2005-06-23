// PR c++/19317
// If we do both NRV and caller-side return slot opt for ma = ma.f,
// constructing fa incorrectly sets ma.i to 0.

extern "C" void abort();

struct A
{
  int i;
  int pad[32];			// force return in memory
  A(): i(0) {}
  A(int ia): i(ia) {}
};

A ga(42);

A f()
{
  A la;
  if (ga.i != 42)
    abort();
  return la;
}

int main()
{
  ga = f ();
}
