// { dg-do link }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR127: Ambiguity in description of matching deallocation function

#include <cstddef>
#include <new>

struct A 
{
  // placement new, but can be called through normal new syntax.
  void* operator new(std::size_t size, float = 0.0f)
  {
    return ::operator new(size);
  }

  // The matching deallocation function must be called, which means
  //  the placemente delete.
  void operator delete(void*);
  void operator delete(void*, float) {}

  A()
  { throw 5; }
};

int main()
{
  (void)new A;
}
