// { dg-do compile { target c++17 } }
// { dg-options "-fno-inline -O0 -fabi-compat-version=0" }

inline auto var = [] () {return 2;};

int bob ()
{
return var ();
}

struct Foo
{
  static inline auto bar = [] () {return 4;};
};

int bill ()
{
  return Foo::bar ();
}

// this one should have internal linkage (from svar)
static auto svar = [] () {return 8;};
int thorn ()
{
  return svar ();
}

// { dg-final { scan-assembler "_ZNK3varMUlvE_clEv:" } }
// { dg-final { scan-assembler "_ZNK3Foo3barMUlvE_clEv:" } }
// { dg-final { scan-assembler-not "_ZNK3FooUlvE_clEv:" } }
