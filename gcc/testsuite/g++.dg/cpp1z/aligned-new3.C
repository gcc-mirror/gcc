// { dg-options -std=c++1z }
// { dg-do run }

#include <new>

struct alignas(64) A {
  int i;
};

bool deleted = false;
void operator delete (void *p, std::size_t, std::align_val_t)
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
