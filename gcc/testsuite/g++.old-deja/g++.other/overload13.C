// { dg-do assemble  }
// Origin: Nathan Sidwell <nathan@codesourcery.com>

struct A {
  bool operator== (A const &) const;
  operator bool () const;
  operator int * () const;
};

bool foo (A &a1, A &a2)
{
  return a1 == a2;
}
