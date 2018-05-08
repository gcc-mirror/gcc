// PR c++/80691
// { dg-do compile { target c++11 } }

struct true_type { static constexpr bool value = true; };
struct false_type { static constexpr bool value = false; };
template<typename...> using void_t = void;
template<typename T> T&& declval();

template<typename T, typename U, typename = void>
struct is_nonnarrowing_conversion : false_type {};

template<typename T, typename U>
struct is_nonnarrowing_conversion<T, U,
    void_t<decltype(T{ declval<U>() })>> : true_type {};

template<typename T>
class wrapper
{
public:
    wrapper(T) {}
};

static_assert(!is_nonnarrowing_conversion<int, float>::value, "");
static_assert(!is_nonnarrowing_conversion<wrapper<int>, float>::value, "");
