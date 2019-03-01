// PR c++/88419
// { dg-do compile { target c++17 } }

template<class> struct ref_view {
  template<class T> ref_view(T&&);
};

template<class R> ref_view(R&) -> ref_view<R>;

struct ref_fn {
  template<class R> auto operator()(R r) const
    noexcept(noexcept(ref_view{r}));
};

template<class R> struct indirect_view {
  indirect_view(R);
};

struct indirect_fn {
  template<class R> auto operator()(R r) const
    noexcept(noexcept(indirect_view{r}));
};
