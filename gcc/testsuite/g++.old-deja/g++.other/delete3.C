// { dg-do run  }
#include <new>

int i;

extern "C" int printf (const char *, ...);

template <class T, class U> 
struct map {
  ~map ();
};

template <class T, class U>
map<T, U>::~map ()
{}

struct SomeClass { };

void* operator new(size_t numBytes, SomeClass&, const std::nothrow_t&) throw()
{
  return operator new(numBytes, std::nothrow);
}

void operator delete(void* pMemory, SomeClass&, const std::nothrow_t&) throw()
{
  i = 7;
  return operator delete(pMemory);
}

int
main()
{
  map< int, int>* pMap = new map< int, int>;
  
  delete pMap;
  
  if (i == 7)
    return 1;
}
