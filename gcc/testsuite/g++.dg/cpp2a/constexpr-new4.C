// P0784R7
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct S
{
  constexpr S () : s (0) { s++; }
  constexpr S (int x) : s (x) { s += 2; }
  constexpr ~S () { if (s != 35) asm (""); s = 5; }
  int s;
};

constexpr bool
foo ()
{
  S *p = new S (7);
  if (p->s != 9) return false;
  p->s = 35;
  delete p;
  p = new S[3] { 11, 13, 15 };
  if (p[0].s != 13 || p[1].s != 15 || p[2].s != 17) return false;
  p[0].s = 35;
  p[2].s = 35;
  p[1].s = 35;
  delete[] p;
  return true;
}

constexpr bool a = foo ();
static_assert (a);
