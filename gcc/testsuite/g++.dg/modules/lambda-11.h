// PR c++/122015

template <typename> auto declval();
template <typename... _BindArgs> void bind_front() {
  [... __bound_args(_BindArgs{})] {};
}
