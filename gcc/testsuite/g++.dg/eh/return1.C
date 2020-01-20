// PR c++/33799
// { dg-do run }

extern "C" void abort();

int c, d;

#if __cplusplus >= 201103L
#define THROWS noexcept(false)
#else
#define THROWS
#endif

struct X
{
  X(bool throws) : throws_(throws) { ++c; }
  X(const X& x) : throws_(x.throws_) { ++c; }
  ~X() THROWS
  {
    ++d;
    if (throws_) { throw 1; }
  }
private:
  bool throws_;
};

X f()
{
  X x(true);
  return X(false);
}

X g()
{
  return X(true),X(false);
}

void h()
{
#if __cplusplus >= 201103L
  []{ return X(true),X(false); }();
#endif
}

int main()
{
  try { f(); }
  catch (...) {}

  try { g(); }
  catch (...) {}

  try { h(); }
  catch (...) {}

  if (c != d)
    throw;
}
