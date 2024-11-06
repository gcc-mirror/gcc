// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target c++11 } }

#ifndef RANGE_FOR_EXT_TEMPS
#define RANGE_FOR_EXT_TEMPS (__cpp_range_based_for >= 202211L)
#else
#if __cplusplus >= 201703L
#if RANGE_FOR_EXT_TEMPS
static_assert (__cpp_range_based_for >= 202211L, "");
#else
static_assert (__cpp_range_based_for >= 201603L
	       && __cpp_range_based_for < 202211L, "");
#endif
#else
static_assert (__cpp_range_based_for >= 200907L
	       && __cpp_range_based_for < 201603L, "");
#endif
#endif

extern "C" void abort ();
void check (bool);

struct S
{
  S () { ++s; }
  S (const S &) { ++s; }
  ~S () { check (true); --s; }
  static int s;
};

int S::s = -1;
S sv;

struct T
{
  T (const S &, const S &) { ++t; }
  T (const T &) { ++t; }
  ~T () { check (false); --t; }
  static int t;
};

int T::t = -1;
T tv (sv, sv);
int a[4];
int c;

void
check (bool is_s)
{
  if (c)
    {
      if (is_s)
	{
	  if (T::t != (c == 1))
	    abort ();
	}
      else
	{
	  if (S::s != (c == 1 ? 0 : 2))
	    abort ();
	}
    }
}

template <typename T>
int *
begin (const T &)
{
  return &a[0];
}

template <typename T>
int *
end (const T &)
{
  return &a[4];
}

const S &
foo (const S &)
{
  return sv;
}

const T &
foo (const T &)
{
  return tv;
}

void
bar ()
{
  if (S::s != 0)
    abort ();
  for (auto x : S ())
    {
      if (S::s != 1)
	abort ();
    }
  if (S::s != 0)
    abort ();
  for (auto x : foo (S ()))
    {
      if (S::s != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0)
    abort ();
  if (T::t != 0)
    abort ();
  c = 1 + RANGE_FOR_EXT_TEMPS;
  for (auto x : T (S (), S ()))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 2;
  for (auto x : foo (T (S (), S ())))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	  || T::t != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 0;
}

template <int N>
void
baz ()
{
  if (S::s != 0)
    abort ();
  for (auto x : S ())
    {
      if (S::s != 1)
	abort ();
    }
  if (S::s != 0)
    abort ();
  for (auto x : foo (S ()))
    {
      if (S::s != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0)
    abort ();
  if (T::t != 0)
    abort ();
  c = 1 + RANGE_FOR_EXT_TEMPS;
  for (auto x : T (S (), S ()))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 2;
  for (auto x : foo (T (S (), S ())))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	  || T::t != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 0;
}

template <typename S, typename T>
void
qux ()
{
  if (S::s != 0)
    abort ();
  for (auto x : S ())
    {
      if (S::s != 1)
	abort ();
    }
  if (S::s != 0)
    abort ();
  for (auto x : foo (S ()))
    {
      if (S::s != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0)
    abort ();
  if (T::t != 0)
    abort ();
  c = 1 + RANGE_FOR_EXT_TEMPS;
  for (auto x : T (S (), S ()))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 2;
  for (auto x : foo (T (S (), S ())))
    {
      if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	  || T::t != RANGE_FOR_EXT_TEMPS)
	abort ();
    }
  if (S::s != 0 || T::t != 0)
    abort ();
  c = 0;
}

int
main ()
{
  bar ();
  baz <0> ();
  qux <S, T> ();
}
