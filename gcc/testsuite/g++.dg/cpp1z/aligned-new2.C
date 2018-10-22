// { dg-do run { target c++17 } }

#include <new>

struct alignas(64) A {
  int i;
  A() { throw 42; }
};
struct B { int i; } b;

void *operator new (std::size_t s, std::align_val_t a, B b)
{
  return operator new (s, a);
}

bool deleted = false;
void operator delete (void *p, std::align_val_t, B)
{
  deleted = true;
}

int main()
{
  try {
    A *p = new (b) A;
    __builtin_abort ();
  } catch (...) {}
  if (!deleted)
    __builtin_abort ();
}
