// PR c++/45080
// { dg-options -std=c++0x }

typedef void(*pfn)();

template<typename=int>
void f()
{
  pfn fn = []{};
}

void test()
{
  f();
}
