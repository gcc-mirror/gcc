/* { dg-do run } */
/* { dg-additional-options "-ftree-slp-vectorize -fno-vect-cost-model" } */

#include <cstddef>

struct A
{
  void * a;
  void * b;
};

struct alignas(16) B
{
  void * pad;
  void * misaligned;
  void * pad2;

  A a;

  void Null();
};

void B::Null()
{
  a.a = nullptr;
  a.b = nullptr;
}

void __attribute__((noinline,noclone))
NullB(void * misalignedPtr)
{
  B* b = reinterpret_cast<B*>(reinterpret_cast<char *>(misalignedPtr) - offsetof(B, misaligned));
  b->Null();
}

int main()
{
  B b;
  NullB(&b.misaligned);
  return 0;
}
