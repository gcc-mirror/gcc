// PR c++/29729

template<typename T> void foo(T)
{
  struct A
  {
    template<int> struct B // { dg-error "local class" }
    {
      typedef B<0> C;
    }
  };
}
