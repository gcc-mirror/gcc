// { dg-do run }

#include <new>

void* operator new(std::size_t sz)
{
  throw std::bad_alloc{};
}

int __attribute__((noipa)) foo ()
{
  int* p1 = static_cast<int*>(::operator new(sizeof(int)));
  return 10;
}

int main()
{
  int res;
  try
    {
      res = foo ();
    }
  catch (...)
    {
      return 0;
    }

  if (res != 10)
    __builtin_abort ();
  return 0;
}
