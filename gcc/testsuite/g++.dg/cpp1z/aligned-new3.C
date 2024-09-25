// { dg-do run { target c++17 } }
// { dg-options "" }

#include <new>

struct alignas(64) A {
  int i;
};

void* operator new (std::size_t n, std::align_val_t)
{
  return operator new (n);
}

bool deleted = false;
void operator delete (void *p, std::size_t, std::align_val_t) // { dg-warning "exception specifier" }
{
  deleted = true;
  operator delete (p);
}

int main()
{
  A *p = new A;
  delete p;
  if (!deleted)
    __builtin_abort();
}
