struct S;

template <S* (S::*p)()>
struct F {
  S* f (S& s)
    {
      return (s.*p)();
    }
};

template <int S::*p>
struct D {
  void d (S& s)
    {
      (s.*p) = 3;
    }
};

struct S {
  S* g ();
  int i;
  F<&S::g> fg;
  D<&S::i> di;
  S* h(), k(F<&S::h>);
  F<&S::g> fg2;
  D<&S::i> di2;
};

S* S::g()
{
  return this;
}

S* S::h()
{
  return this;
}

int main()
{
  S s;
  s.i = 2;
  s.di.d (s);
  if (s.i != 3)
    return 1;
  if (s.fg2.f(s) != &s)
    return 1;
}
