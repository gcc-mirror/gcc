// PR c++/67164
// { dg-do compile { target c++11 } }

#include <type_traits>

namespace detail {
    template <bool ...b>
    struct fast_and
        : std::is_same<fast_and<b...>, fast_and<(b, true)...>>
    { };
}

template <typename ...Xn>
struct tuple {
    tuple() { }

    template <typename ...Yn, typename = typename std::enable_if<
        detail::fast_and<std::is_constructible<Xn, Yn&&>::value...>::value
    >::type>
    tuple(Yn&& ...yn) { }

    template <typename ...Yn, typename = typename std::enable_if<
        detail::fast_and<std::is_constructible<Xn, Yn const&>::value...>::value
    >::type>
    tuple(tuple<Yn...> const& other) { }
};

tuple<tuple<>> t{};
tuple<tuple<>> copy = t;
