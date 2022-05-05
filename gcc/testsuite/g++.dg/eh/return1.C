// PR c++/33799
// { dg-do run }

extern "C" void abort();

int c, d;

#if __cplusplus >= 201103L
#define THROWS noexcept(false)
#else
#define THROWS
#endif

extern "C" int printf (const char *, ...);
#define DEBUG // printf ("%p %s\n", this, __PRETTY_FUNCTION__)

struct X
{
  X(bool throws) : throws_(throws) { ++c; DEBUG; }
  X(const X& x); // not defined
  ~X() THROWS
  {
    ++d; DEBUG;
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

X i()
{
  try {
    X x(true);
    return X(false);
  } catch(...) {}
  return X(false);
}

X j()
{
  try {
    return X(true),X(false);
  } catch(...) {}
  return X(false);
}

template <class T>
T k()
{
  try {
    return T(true),T(false);
  } catch (...) {}
  return T(true),T(false);
}

X l() try { return X(true),X(false); }
  catch (...) { return X(true),X(false); }

template <class T>
T m()
  try { return T(true),T(false); }
  catch (...) { return T(true),T(false); }

int main()
{
  try { f(); }
  catch (...) {}

  try { g(); }
  catch (...) {}

  try { h(); }
  catch (...) {}

  try { i(); }
  catch (...) {}

  try { j(); } catch (...) {}

  try { k<X>(); } catch (...) {}

  try { l(); } catch (...) {}
  try { m<X>(); } catch (...) {}

  return c - d;
}
