// P2128R6
// { dg-do run }
// { dg-options "-std=c++23" }

extern "C" void abort ();

struct S
{
  constexpr S () : a {} {};
  constexpr S (int x, int y, int z) : a {x, y, z} {};
  constexpr int &operator[] () { return a[0]; }
  constexpr int &operator[] (int x) { return a[x]; }
  constexpr int &operator[] (int x, long y) { return a[x + y * 8]; }
  int a[64];
};

struct T
{
  operator int () { return 42; };
};

int buf[64];

struct U
{
  operator int * () { return buf; }
};

static_assert (S ()[1] == 0);
static_assert (S (1, 2, 42)[2] == 42);
static_assert (S ()[3, 4] == 0);
static_assert (S (1, 43, 2)[1, 0] == 43);
static_assert (S ()[] == 0);
static_assert (S (44, 1, 2)[] == 44);

int
main ()
{
  S s;
  for (int i = 0; i < 64; i++)
    s.a[i] = 64 - i;
  if (s[] != 64 || s[3] != 61 || s[4, 5] != 20)
    abort ();
  s[]++;
  s[42]++;
  ++s[3, 2];
  if (s.a[0] != 65 || s.a[42] != 23 || s.a[19] != 46)
    abort ();
  T t;
  U u;
  if (&u[t] != &buf[42])
    abort ();
  if (&t[u] != &buf[42])
    abort ();
}
