// PR c++/45080
// { dg-do compile { target c++11 } }

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
