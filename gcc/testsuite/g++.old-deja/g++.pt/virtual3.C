// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <typename X>
struct S : virtual public X
{
  int i;
};

template <typename T>
struct X : virtual public T, virtual public S<T>
{
  int i;

  X () : i (3) {}
};
