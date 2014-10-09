// PR c++/60409
// { dg-do compile { target c++14 } }

struct A
{
  void foo();
};

template<typename T> void bar(T)
{
  (A().foo)();
}
