// PR c++/97412
// { dg-do compile { target c++20 } }

template <class T, class... TArgs>
concept call_bar_with = requires(T t, TArgs... args) {
  t.bar(args...);
};

template <class T, class... TArgs>
concept foo = requires {
  requires call_bar_with<T, TArgs...>;
};
