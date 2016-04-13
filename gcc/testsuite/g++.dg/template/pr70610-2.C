// PR c++/70610
// { dg-do link }

struct A { };

void operator+ (const A &, A &);
void operator+ (A &, const A &);
void operator+ (const A &, const A &) { }

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
