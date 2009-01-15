// PR c++/37646

struct A
{
  void foo();

  void bar(int i)
  {
    void (*p)() = i ? foo : foo; // { dg-error "invalid use of member" }
  }
};
