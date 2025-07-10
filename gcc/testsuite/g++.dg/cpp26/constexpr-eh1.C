// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct S {
  constexpr S () : s (new int (0)) {}
  constexpr S (int x) : s (new int (x)) {}
  constexpr S (const S &x) : s (new int (*x.s)) {}
  constexpr ~S () { delete s; }
  int *s;
};
struct T : public S {
  constexpr T () : S () {}
  constexpr T (int x) : S (x) {}
  constexpr T (const T &x) : S (*x.s) {}
  constexpr ~T () {}
};
struct U : public T {
  constexpr U () : T () {}
  constexpr U (int x) : T (x) {}
  constexpr U (const U &x) : T (*x.s) {}
  constexpr ~U () {}
};
struct V : public T {
  constexpr V () : T () {}
  constexpr V (int x) : T (x) {}
  constexpr V (const U &x) : T (*x.s) {}
  constexpr ~V () {}
};

template <typename X>
constexpr int
foo (X x)
{
  try { throw x; }
  catch (int &a) { return 42 + a; }
  catch (const unsigned b) { return 43 + b; }
  catch (const long &c) { return 44 + c; }
  catch (bool d) { return 45 + d; }
  catch (const U &e) { return 46 + *e.s; }
  catch (const T &f) { return 47 + *f.s; }
  catch (S g) { return 48 + *g.s; }
  catch (int *const &h) { return 49; }
  catch (long long *) { return 50; }
  catch (const S *const &) { return 51; }
  catch (...) { return 52; }
}

template <typename X>
constexpr int
bar (const X &x)
{
  throw x;
}

template <typename X>
constexpr int
baz (const X &x)
{
  try
    {
      try { bar (x); }
      catch (int &a) { a += 80; throw; }
      catch (long b) { b += 80; throw; }
      catch (U &c) { c.s[0] += 82; throw; }
      catch (V d) { d.s[0] += 83; throw; }
    }
  catch (int a) { return 42 + a; }
  catch (const long &b) { return 43 + b; }
  catch (S &c) { return 44 + c.s[0]; }
  catch (long long d) { return 45 + d; }
  catch (...) { return -1; }
}

constexpr int
qux (int x, bool y = true)
{
  try
    {
      switch (x)
	{
	case 0: throw 42; break;
	case 1: x = y ? throw 43 : 5; break;
	case 2: x = -(throw 44, 6); break;
	case 3: x = x + (throw 45, 7); break;
	case 4: x = (throw 46, 8) + x; break;
	case 5: x = (throw 47, y) ? 4 : 5; break;
	case 6: x += (throw 48, y); break;
	case 7: x = (double) (throw 49, y); break;
	case 8: x = foo ((throw 50, x)); break;
	default: break;
	}
    }
  catch (int a) { return a; }
  return -1;
}

constexpr int
corge ()
{
  try { throw 0; }
  catch (int *const &h) { return 49; }
  catch (long long *) { return 50; }
  catch (const S *const &) { return 51; }
  catch (...) { return 52; }
}

static_assert (foo (12) == 54);
static_assert (foo (12U) == 55);
static_assert (foo (12L) == 56);
static_assert (foo (false) == 45);
static_assert (foo (true) == 46);
static_assert (foo (U (12)) == 58);
static_assert (foo (T (20)) == 67);
static_assert (foo (S (30)) == 78);
static_assert (foo (nullptr) == 49);
static_assert (foo ((int *)nullptr) == 49);
static_assert (foo ((long long *)nullptr) == 50);
static_assert (foo ((const S *)nullptr) == 51);
static_assert (foo ((const T *)nullptr) == 51);
static_assert (foo ((const U *)nullptr) == 51);
static_assert (foo (12ULL) == 52);
static_assert (baz (5) == 127);
static_assert (baz (6L) == 49);
static_assert (baz (U (25)) == 151);
static_assert (baz (V (26)) == 70);
static_assert (baz (T (27)) == 71);
static_assert (baz (S (28)) == 72);
static_assert (baz (7LL) == 52);
static_assert (baz (8ULL) == -1);
static_assert (qux (0) == 42);
static_assert (qux (1) == 43);
static_assert (qux (2) == 44);
static_assert (qux (3) == 45);
static_assert (qux (4) == 46);
static_assert (qux (5) == 47);
static_assert (qux (6) == 48);
static_assert (qux (7) == 49);
static_assert (qux (8) == 50);
static_assert (corge () == 52);
