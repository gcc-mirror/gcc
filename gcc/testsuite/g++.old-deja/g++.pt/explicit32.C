// { dg-do assemble  }
// GROUPS passed templates
template <class T>
struct S
{
};


template <>
struct S<int>
{
  void foo();
};


void S<int>::foo()
{
}


void bar()
{
  S<int> si;
  si.foo();
}
