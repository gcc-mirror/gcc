// PR c++/19034

template< bool C > struct B
{
};

template<typename S> int foo();
template<typename S> int foo1();

template<typename T> struct bar : public B <(sizeof(foo<T>()) == 1)>
{
};

template<typename T> struct bar1 : public B <(sizeof(foo1<T>()) == 1)>
{
};
