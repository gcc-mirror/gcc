// PR c++/63885
// { dg-do compile { target c++11 } }

template<class T> struct remove_reference { typedef T type; };
template<class T> struct remove_reference<T&> { typedef T type; };
template<class T> struct remove_reference<T&&> { typedef T type; };

template<class T> struct is_lvalue_reference { static const bool value = false; };
template<class T> struct is_lvalue_reference<T&> { static const bool value = true; };

template <bool B, class U, class V> struct conditional;
template <class U, class V> struct conditional<true, U, V> { typedef U type; };
template <class U, class V> struct conditional<false, U, V> { typedef V type; };

template<typename _Tp> constexpr _Tp&&
forward(typename remove_reference<_Tp>::type& __t) noexcept
{ return static_cast<_Tp&&>(__t); }

///////////////////////////////////////////////////////////////////////////////

template <typename C> struct member_forward
{
    typedef typename remove_reference <C>::type::type T;
    typedef typename conditional
    <
        is_lvalue_reference <C &&>::value,
        T&,
        T
    >::type type;
};

template <typename C> using member_forward_t = typename member_forward <C>::type;

///////////////////////////////////////////////////////////////////////////////

template <int  , typename  > struct __get;
template <       typename T> struct __get <0, T>
{
    constexpr static auto value (T arg)
     -> decltype ((forward <member_forward_t <T>> (arg.t)))
    {
        return     forward <member_forward_t <T>> (arg.t);
    }
};

template <int N, typename T> constexpr auto get (T && arg)
 -> decltype (__get <N, T &&>::value (forward <T> (arg)))
{
    return    __get <N, T &&>::value (forward <T> (arg));
}

///////////////////////////////////////////////////////////////////////////////

template <typename T> struct S
{
    typedef T type;
    T t;

    template <typename U> constexpr S (U && u) : t (forward <U> (u)) {}
};
static_assert (get <0> (S <int &&> (1)) == 1, ""); // g++ 4.9 passes, g++ trunk r217559 fails
