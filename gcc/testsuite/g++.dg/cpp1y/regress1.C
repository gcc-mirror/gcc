// PR c++/60409
// { dg-options -std=c++1y }

struct A
{
  void foo();
};

template<typename T> void bar(T)
{
  (A().foo)();
}
