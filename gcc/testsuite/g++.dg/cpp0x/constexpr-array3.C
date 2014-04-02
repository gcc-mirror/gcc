// PR c++/48132
// { dg-do compile { target c++11 } }

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
