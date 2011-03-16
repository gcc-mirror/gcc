// PR c++/48132
// { dg-options -std=c++0x }

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
