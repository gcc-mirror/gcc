// PR c++/122070
// { dg-do compile { target c++23 } }

namespace std {
    using size_t = decltype(sizeof(0));
    template<typename CharT>
    struct basic_string_view {
        const CharT* ptr;
        size_t len;
        constexpr basic_string_view(const CharT* p) : ptr(p), len(0) { while (p && p[len]) ++len; }
    };
    using string_view = basic_string_view<char>;
}

template<std::size_t N>
struct sized_string_view: std::string_view {
    using std::string_view::string_view;
};
template<std::size_t N>
sized_string_view(const char (&str)[N]) -> sized_string_view<N - 1>;

constexpr auto string_builder(auto first, auto second, auto... trailing) {
    constexpr auto is_last = sizeof...(trailing) == 0;
    auto buffer = 1;
    if constexpr (is_last) {
        return buffer;
    } else
        return string_builder(buffer, trailing...);
}

constexpr auto copyright = string_builder(sized_string_view("a"), sized_string_view("b"), sized_string_view("c"));
