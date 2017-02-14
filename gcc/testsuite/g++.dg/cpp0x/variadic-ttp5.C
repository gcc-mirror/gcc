// { dg-do compile { target c++11 } }

template <typename, typename> struct A { };
template <typename T> struct B { };

template <typename T, template <T...> class C, T... Is>
struct A<B<T>, C<Is...>>
{
  using type = C<Is...>;
};
