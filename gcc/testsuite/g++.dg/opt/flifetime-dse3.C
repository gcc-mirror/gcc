// PR c++/68006
// { dg-do run { target c++11 } }
// { dg-options -O2 }

inline void* operator new(__SIZE_TYPE__, void* ptr)
{
  return ptr;
}

struct X { int x; int y; int z = 42; };

void test_bar(void* p)
{
  new(p) X{};   // Bad.
}

int main()
{
  int ar[3] = { 1,2,3 };
  test_bar (ar);
  return (ar[0] != 0 || ar[1] != 0 || ar[2] != 42);
}
