// PR c++/54777
// { dg-do compile { target c++11 } }

struct S
{
  int s[1];
  constexpr const int &foo (unsigned i) const { return (i < 1 ? 0 : throw 1), s[i]; }
  constexpr const int &bar (unsigned i) const { return i < 1 ? s[i] : (throw 0, s[i]); }
};

int
main ()
{
  constexpr S a {};
  constexpr int i = a.foo (0);
  constexpr int j = a.bar (0);
  static_assert (i == j, "Ouch");
}
