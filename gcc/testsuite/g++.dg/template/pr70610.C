// PR c++/70610
// { dg-do link }

struct A { };

void operator+ (A &);
void operator+ (const A &) { }


template <typename T>
void
foo ()
{
  +A ();
}

int
main ()
{
  foo<int> ();
}
