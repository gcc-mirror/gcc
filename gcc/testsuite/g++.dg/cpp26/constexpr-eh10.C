// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct S {
};
struct T {
  constexpr ~T () noexcept (false) { throw S {}; }
};
struct U {
  int u;
};
struct V {
  int v;
  constexpr V (int x)
  try : v { x }
  {
    if (v > 42)
      throw U { 42 };
  }
  catch (U &u)
  {
    --u.u;
  }
};
struct W {
  constexpr ~W () { ++w; }
  int &w;
};
struct X : public V {
  constexpr X (int x)
  try : V(x)
  {
  }
  catch (U &u)
  {
    --u.u;
  }
};

constexpr int
foo (bool x)
{
  try
    {
      T t;				// { dg-error "'std::terminate' called after throwing an exception '42'" }
      if (x)				// { dg-message "destructor exited with an exception" "" { target *-*-* } .-1 }
	throw 42;
      return 10;
    }
  catch (S)
    {
      return 11;
    }
}

constexpr int
bar ()
{
  V v { 42 };
  try
    {
      V w { 43 };
    }
  catch (const U &u)
    {
      if (u.u == 41)
	return 44;
    }
  return -1;
}

constexpr int
baz ()
{
  int i = 42;
  try
    {
      W w { i };
      throw S ();
    }
  catch (...)
    {
      if (i == 43)
	return 42;
    }
  return -1;
}

constexpr int
qux ()
{
  X v { 42 };
  try
    {
      X w { 43 };
    }
  catch (const U &u)
    {
      if (u.u == 40)
	return 48;
    }
  return -1;
}

static_assert (foo (false) == 11);
constexpr int a = foo (true);		// { dg-message "in 'constexpr' expansion of" }
static_assert (bar () == 44);
static_assert (baz () == 42);
static_assert (qux () == 48);
