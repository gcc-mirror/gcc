// PR c++/70610
// { dg-do link }

struct A { void operator+ (const A &) { }; };

void operator+ (const A &, A &);

template <typename T>
void
foo ()
{
  A () + A ();
}

int
main ()
{
  foo<int> ();
}
