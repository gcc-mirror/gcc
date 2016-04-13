// PR c++/70610
// { dg-do link }

void bar (const int &, int &);
void bar (int &, const int &);
void bar (const int &, const int &) { }

int a, b;

template <typename T>
void
foo ()
{
  bar (a + 1, b + 2);
}

int
main ()
{
  foo<int> ();
}
