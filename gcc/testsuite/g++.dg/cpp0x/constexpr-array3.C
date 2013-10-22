// PR c++/48132
// { dg-options -std=c++11 }

struct C
{
  constexpr C (int x) : c (x) {}
  int c;
};

void
foo ()
{
  C a[] = { C (0) };
}
