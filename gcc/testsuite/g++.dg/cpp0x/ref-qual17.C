// { dg-do compile { target c++11 } }

struct A
{
  void foo () &;
};

void
bar (__UINTPTR_TYPE__ a)
{
  reinterpret_cast<A *>(a)->foo ();
}
