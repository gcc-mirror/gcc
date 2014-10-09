// PR c++/62072
// { dg-do compile { target c++11 } }

template<typename T> struct tuple_size {};
template<typename T> struct tuple_size<T const> : tuple_size<T> {};

template<typename T, typename = void>
struct query {
    static constexpr bool value = false;
};
template<typename T>
struct query<T, typename tuple_size<T>::type> {
    static constexpr bool value = true;
};

// fine
static_assert( !query<int>::value, "" );
static_assert( !query<int const>::value, "" );

// error: invalid use of incomplete type 'struct tuple_size<void()>'
static_assert( !query<void()>::value, "" );
