// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int i;
};

template <typename T>
struct X : virtual public T, virtual public S
{
  int i;

  X () : i (3) {}
};
