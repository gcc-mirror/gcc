// Addr of function from multiple namespaces

namespace X
{
  void Foo (int);
  void Foo (short);
}

namespace Y
{
  void Foo (float);
  void Foo (double);
}

template <typename T> void Foo (T *);

using namespace X;

using namespace Y;

void (*(Baz ())) (float)
{
  return Foo;
}

void (*(Bar ())) (void *)
{
  return Foo;
}
