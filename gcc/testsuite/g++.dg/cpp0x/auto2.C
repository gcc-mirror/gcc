// Positive test for auto
// { dg-do run { target c++11 } }

#include <typeinfo>
extern "C" void abort();

int f() { return 0; }

struct A
{
  int i;
  int f() { return 0; }
  A operator+(A a) { return a; }
};

template <class T>
void g(T t)
{
  auto x = t+t;
  if (typeid(x) != typeid(t+t))
    abort();

  auto p = new auto(&t);
  if (typeid(p) != typeid(T**))
    abort();
}

int main()
{
  auto i = 42;
  if (typeid (i) != typeid (int))
    abort();

  auto *p = &i;
  if (typeid (p) != typeid (int*))
    abort();

  auto *p2 = &p;
  if (typeid (p2) != typeid (int**))
    abort();

  auto (*fp)() = f;
  if (typeid (fp) != typeid (int (*)()))
    abort();

  auto A::* pm = &A::i;
  if (typeid (pm) != typeid (int A::*))
    abort();

  auto (A::*pmf)() = &A::f;
  if (typeid (pmf) != typeid (int (A::*)()))
    abort();

  g(42);
  g(10.f);
  g(A());

  auto *p3 = new auto (i);
  if (typeid (p3) != typeid (int*))
    abort();

  for (auto idx = i; idx != 0; idx = 0);
  while (auto idx = 0);
  if (auto idx = 1);

  switch (auto s = i)
    {
    case 42:
      break;
    }

  auto j = 42, k = 24;
  return 0;
}
