// PR c++/117501
// { dg-do run { target c++20 } }

constexpr int
twiddle (int i)
{
 if (__builtin_is_constant_evaluated ())
    return 3;
  return i;
}
struct S {
  constexpr S(int i) : i{twiddle (i)} {}
  int i;
};
struct Q {
  consteval Q(S s_) : s{s_, s_} {}
  S s[2];
};
int
main ()
{
  Q q(twiddle (42));
  if (q.s[0].i != 3 || q.s[1].i != 3)
    __builtin_abort ();
}
