// PR c++/98388
// { dg-do compile { target c++11 } }

struct moveonly {
    moveonly() = default;
    moveonly(moveonly&&) = default;
};

template<class T>
constexpr auto is_throwable(T t) -> decltype(throw t, true) {
    return true;
}
template<class T>
constexpr bool is_throwable(...) { return false; }

constexpr bool b = is_throwable<moveonly>(moveonly{});
#if __cplusplus >= 202002L
static_assert (b, "move from the function parameter");
#else
static_assert (!b, "no move from the function parameter");
#endif
