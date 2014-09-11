// PR c++/63139
// { dg-do compile { target c++11 } }

template<typename ...T>
struct type_list {};

template<typename ...T>
struct make_type_list
{
    using type = type_list<T...>;
};

// The bug disappears if you use make_type_list directly.
template<typename ...T>
using make_type_list_t = typename make_type_list<T...>::type;


struct ContainerEndA {};

template<typename ...Ts>
struct ContainerA
{
    using type = make_type_list_t<Ts..., ContainerEndA>;
};


struct ContainerEndB {};

template<typename ...Ts>
struct ContainerB
{
    using type = make_type_list_t<Ts..., ContainerEndB>;
};

template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

#define SA(X) static_assert((X), #X)

SA((is_same<ContainerB<>::type, type_list<ContainerEndB>>::value));
SA((!is_same<ContainerA<>::type, type_list<ContainerEndB>>::value));
SA((!is_same<ContainerA<>::type, ContainerB<>::type>::value));
