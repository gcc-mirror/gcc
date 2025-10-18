// PR c++/122310
struct Foo {
  constexpr static inline auto do_nothing = [](auto && ...){};
  using TNothing = decltype(do_nothing);
};

template <typename T>
struct X {
  struct Inner {
    union MoreInner {
      static constexpr auto x = []{};
#if __cplusplus >= 202002L
      static decltype([]{}) y;
#endif
    };
  };

  using A = decltype(Inner::MoreInner::x);
#if __cplusplus >= 202002L
  using B = decltype(Inner::MoreInner::y);
#endif
};

inline X<int>::A* a{};
#if __cplusplus >= 202002L
inline X<int>::B* b{};
#endif
