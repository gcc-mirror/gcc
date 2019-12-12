// PR c++/87366
// { dg-do compile { target c++11 } }

struct A {};
struct B {};

template <typename T> struct wrapper {};

template <typename> struct enable_if_A { };
template<> struct enable_if_A<A> { using type = void; };

template <typename T, typename = typename enable_if_A<T>::type> using ok_t = T;

template <typename T> void not_even_called(wrapper<const ok_t<T>&> a);

template <typename T> int called(wrapper<const T&> a);

void test(wrapper<const B&>& val)
{
    called(val);
}
