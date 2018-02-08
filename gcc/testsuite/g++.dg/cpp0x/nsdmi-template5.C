// PR c++/58599
// { dg-do compile { target c++11 } }

template<int> struct A1;

template<> struct A1<0>
{
  template<typename, typename...> struct B1
  {
    template<typename> int foo1() { return 0; }

    int i1 = foo1<int>();
  };
};

template<int> struct A2;

template<> struct A2<0>
{
  template<typename, typename> struct B2
  {
    template<typename> int foo2() { return 1; }

    int i2 = foo2<int>();
  };
};

template<int> struct A3;

template<> struct A3<0>
{
  template<typename> struct B3
  {
    template<typename> int foo3() { return 2; }

    int i3 = foo3<int>();
  };
};
