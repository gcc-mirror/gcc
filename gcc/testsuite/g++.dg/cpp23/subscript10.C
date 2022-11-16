// PR c++/107624
// { dg-do run { target c++11 } }
// { dg-options "" }

int n[3];
struct S {
  static int &operator[] (int x) { n[0] |= (1 << x); return n[2]; }	// { dg-warning "may be a static member function only with" "" { target c++20_down } }
#if __cpp_multidimensional_subscript >= 202211L
  static int &operator[] () { n[0] |= (1 << 8); return n[2]; }
  static int &operator[] (int y, int z, int w) { n[0] |= (1 << y) | (1 << z) | (1 << w); return n[2]; }
#endif
  int s;
};
volatile S s[2];

S &
foo (int x)
{
  static S t;
  n[1] |= (1 << x);
  return t;
}

int
main ()
{
  int i = 0;
  foo (0) [0]++;
  if (n[0] != 1 || n[1] != 1 || n[2] != 1)
    __builtin_abort ();
  s[i++][2]++;
  if (i != 1 || n[0] != 5 || n[1] != 1 || n[2] != 2)
    __builtin_abort ();
#if __cpp_multidimensional_subscript >= 202211L
  foo (3) []++;
  if (n[0] != 261 || n[1] != 9 || n[2] != 3)
    __builtin_abort ();
  int y = 10;
  int z = 10;
  int w = 13;
  foo (4) [y++, ++z, w++]++;
  if (n[0] != 11525 || n[1] != 25 || n[2] != 4
      || y != 11 || z != 11 || w != 14)
    __builtin_abort ();
#endif
}
