// PR c++/17501

template<int> struct A;

template<> struct A<0>
{
    struct B
    {
        struct C
        {
	  typedef int D;
        };
    };
};

template<int I> struct E
{
  typename A<I>::B::C::D i;
};
