// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++11 } }
// { dg-options "" }

struct S { int s; };
constexpr S c[] = { { 3 }, { 4 }, { 5 }, { 6 }, { 7 } };
struct U {
  constexpr const S *begin () const { return &c[0]; }
  constexpr const S *end () const { return &c[s]; }
  int s;
};
constexpr U u1 = { 3 }, u2 = { 0 };
struct V {
  constexpr V () : a (1), b (2), c (3.0) {}
  constexpr int foo () const { return a; }
  constexpr unsigned long bar () const { return b; }
  constexpr double baz () const { return c; }
  int a;
  unsigned long b;
  double c;
};

long long
foo ()
{
  long long r = 0;
  template for (constexpr auto h = 2; constexpr auto g : u1)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      constexpr auto i = g.s + h;
      r += i;
    }
  template for (constexpr auto h = 42; constexpr auto g : u2)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      constexpr auto i = g.s + h;
      __builtin_abort ();
    }
  return r;
}

long long
bar ()
{
  long long r = 0;
  template for (constexpr S a { 42 }; constexpr auto b : { S { 1 }, S { 3 }, S { 5 } })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      constexpr auto c = a.s + b.s;
      r += c;
    }
  return r;
}

constexpr V v;

long long
baz ()
{
  long long r = 0;
  template for (constexpr auto x = 5; constexpr auto y : v)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      constexpr auto z = x + y;
      r += z;
    }
  return r;
}

int
main ()
{
  if (foo () != 18)
    __builtin_abort ();
  if (bar () != 135)
    __builtin_abort ();
  if (baz () != 21)
    __builtin_abort ();
}
