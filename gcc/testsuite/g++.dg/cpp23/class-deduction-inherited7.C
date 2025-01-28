// PR c++/117855
// { dg-do compile { target c++20 } }

template <typename T, int _Extent = -1> struct span { span(T&&);};
template <typename T> span(T &&) -> span<T>;
template <typename et, int e = -1>
struct this_span : span<et, e> {
  using span<et, e>::span;
};
template <typename T> this_span(T &&) -> this_span<T>;
int vec;
this_span a = vec;
