#include <cstddef>

template <class T>	
struct S {
  void *operator new (size_t);
  void *operator new (size_t, int);
  void operator delete (void*);
};

static void* s[2];

template <>
void* S<int>::operator new (size_t b) 
{ 
  s[0] = ::operator new(b);
  return s[0];
}

template <>
void* S<int>::operator new (size_t b, int)
{
  s[1] = ::operator new(b);
  return s[1];
}

template <>
void S<int>::operator delete (void*) 
{
}

int main()
{
  S<int>* s1 = new S<int>;
  S<int>* s2 = new(3) S<int>;

  if (s1 != s[0] || s2 != s[1])
    return 1;

  delete s1;
  delete s2;
}
