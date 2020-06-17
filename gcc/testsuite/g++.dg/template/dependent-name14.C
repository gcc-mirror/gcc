// PR c++/90711
// { dg-do compile { target c++11 } }

namespace test {
    void EXISTS(int);
}

template<typename... ARGS>
struct stub_void {
    typedef void type;
};
template<typename... ARGS>
using stub_void_t = typename stub_void<ARGS...>::type;

#if !defined(SUPPRESS)
template<typename O, typename = void>
struct has_to_string {
    static constexpr bool value = false;
};

template<typename O>
struct has_to_string<O, stub_void_t<decltype(EXISTS(O{}))>> {
    static constexpr bool value = true;
};
#endif

template<typename O, typename = void>
struct has_std_to_string {
    static constexpr bool value = false;
};

template<typename O>
struct has_std_to_string<O, stub_void_t<decltype(test::EXISTS(O{}))>> {
    static constexpr bool value = true;
};

static_assert (has_std_to_string<int>::value, "");

