// PR c++/77585
// { dg-do run { target c++14 } }

// Confusion about this capture when instantiating generic lambda's
// function operator

template <typename F> int Eat (F &&f) { return f (1); }

struct Foo {
  int x = 1;
  int Share () { return x++; }
  int Frob (int);
};

int Foo::Frob (int r)
{
  auto lam = [&](auto) { return Share (); };
  r += Eat (lam);

  auto lam0 = [&](auto) {
    auto lam1 = [&](auto) { return Share (); };
    return Eat (lam1); };
  r += Eat (lam0);

  return r;
}

int Frob (int r) 
{
  auto lam = [&](auto) { return 1; };
  r += Eat (lam);
  return r;
}


int main ()
{
  Foo f;
  
  return Frob (f.Frob (0)) == 4 ? 0 : 1;
}
