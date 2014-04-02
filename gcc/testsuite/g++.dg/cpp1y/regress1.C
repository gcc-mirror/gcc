// PR c++/60409
// { dg-do compile { target c++1y } }

struct A
{
  void foo();
};

template<typename T> void bar(T)
{
  (A().foo)();
}
