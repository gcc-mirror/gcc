// PR c++/60383

template<int> struct A
{
  template<typename> struct B
  {
    template<typename T> struct B<T*> {};  // { dg-error "specialization" }
  };
};

A<0>::B<char*> b;
