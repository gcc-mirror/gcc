// Test for sensible handling of template-ids with arg-dep lookup.
// This is still an open issue.

namespace N
{
  struct A { };
  void f(void (*)(int, N::A));
}

namespace M
{
  struct B { };
  void f(void (*)(B, N::A));
}

template <class T>
void g(T, N::A);

void g();

int main()
{
  f(g<int>);
  f(g<M::B>);
}
