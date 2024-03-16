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
  X(bool throws) : i(-42), throws_(throws) { ++c; DEBUG; }
  X(const X& x); // not defined
  ~X() THROWS
  {
    i = ++d; DEBUG;
    if (throws_) { throw 1; }
  }
  int i;
private:
  bool throws_;
};

X f()
{
  X x(true);
  return X(false);
}

X f2()
{
 foo:
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

X i2()
{
  try {
  foo:
    X x(true);
    return X(false);
  } catch(...) {}
  return X(false);
}

// c++/112301
X i3()
{
  try {
    X x(true);
    return X(false);
  } catch(...) { throw; }
}

X i4()
{
  try {
    X x(true);
    X x2(false);
    return x2;
  } catch(...) { throw; }
}

X i4a()
{
  try {
    X x2(false);
    X x(true);
    return x2;
  } catch(...) { throw; }
}

X i4b()
{
  X x(true);
  X x2(false);
  return x2;
}

X i4c()
{
  X x2(false);
  X x(true);
  return x2;
}

X i5()
{
  X x2(false);

  try {
    X x(true);
    return x2;
  } catch(...) {
    if (x2.i != -42)
      d += 42;
    throw;
  }
}

X i6()
{
  X x2(false);

  try {
    X x(true);
    return x2;
  } catch(...) {
    if (x2.i != -42)
      d += 42;
  }
  return x2;
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

  try { f2(); } catch (...) {}

  try { g(); }
  catch (...) {}

  try { h(); }
  catch (...) {}

  try { i(); }
  catch (...) {}

  try { i2(); } catch (...) {}
  try { i3(); } catch (...) {}
  try { i4(); } catch (...) {}
  try { i4a(); } catch (...) {}
  try { i4b(); } catch (...) {}
  try { i4c(); } catch (...) {}
  try { i5(); } catch (...) {}
  try { i6(); } catch (...) {}

  try { j(); } catch (...) {}

  try { k<X>(); } catch (...) {}

  try { l(); } catch (...) {}
  try { m<X>(); } catch (...) {}

  return c - d;
}
