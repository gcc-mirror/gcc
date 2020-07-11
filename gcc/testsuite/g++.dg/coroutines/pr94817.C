
void no_coroutine_traits() {
  co_await 4; // { dg-error {coroutines require a traits template\; cannot find 'std::coroutine_traits'} }
}

// check we have not messed up continuation of the compilation.
template <class... Args>
struct void_t_imp {
  using type = void;
};
