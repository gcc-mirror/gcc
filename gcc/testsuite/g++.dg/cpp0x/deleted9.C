// PR c++/64352
// { dg-do compile { target c++11 } }

template<bool B> struct bool_type
{ static constexpr bool value = B; };

using true_type = bool_type<true>;
using false_type = bool_type<false>;

template<typename T> T&& declval();

template<typename...> struct void_ { using type = void; };
template<typename... I> using void_t = typename void_<I...>::type;

template<typename _Tp, typename = void>
struct _Has_addressof_free: false_type { };

template<typename _Tp>
struct _Has_addressof_free
<_Tp, void_t<decltype( operator&(declval<const _Tp&>()) )>>
: true_type { };

struct foo {};
void operator&(foo) = delete;

int main()
{
    static_assert( !_Has_addressof_free<int>::value, "" );
    // error: use of deleted function 'void operator&(foo)'
    static_assert( !_Has_addressof_free<foo>::value, "" );
}
