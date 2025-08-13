// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++14 } }
// { dg-options "" }

struct S { int a; long b; short c; };
struct T { long long a; unsigned b; signed char c; };
struct U { float a; double b; long double c; };
constexpr S d = { 1, 2, 3 }, f = { 7, 8, 9 };
constexpr T j = { 10, 11, 12 };
constexpr U k = { 13.0f, 14.5, 15.5 };

template <typename T>
long long
foo ()
{
  auto s = [] (auto f)
    {
      long long r = 0;
      template for (auto g : { d, T { 4, 5, 6 }, f, j, k })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{
	  r += g.a + g.b + g.c;
	  decltype (g) s = g;
	  r += sizeof (s);
	}
      return r;
    };
  return s (f);
}

int
main ()
{
  if (foo <S> () != 121 + 3 * sizeof (S) + sizeof (T) + sizeof (U))
    __builtin_abort ();
}
