// PR c++/64970
// { dg-do compile { target c++11 } }

template<typename T>
T && declval();

template<typename T>
struct void_ { using type = void; };

template<typename T>
using void_t = typename void_<T>::type;

template<class A, class B>
struct Outer
{
    template<class C, class D>
    using Inner = decltype(true ? declval<C>() : declval<D>());
};

template<class A, class B, typename Enable = void>
struct S
{};

template<class A, class B>
struct S<A, B, void_t<typename Outer<A, B>::template Inner<A, B>>>
{};

struct A{};
struct B{};
int main()
{
    S<A, B> s;
}
