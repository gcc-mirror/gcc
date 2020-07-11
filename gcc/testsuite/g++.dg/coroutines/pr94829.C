
namespace std::experimental {
template <typename R, typename... T> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};

template <class Promise = void> struct coroutine_handle;
template <> struct coroutine_handle<void> {
  static coroutine_handle from_address(void *) noexcept;
  coroutine_handle() = default;
  template <class PromiseType>
  coroutine_handle(coroutine_handle<PromiseType>) noexcept;
};
template <class Promise> struct coroutine_handle : coroutine_handle<void> {
  coroutine_handle() = default;
  static coroutine_handle from_address(void *) noexcept;
};
}

struct suspend_always {
  bool await_ready() noexcept;
  void await_suspend(std::experimental::coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};

struct Task {
  struct promise_type {
    Task get_return_object();
    void return_void() {}
    suspend_always initial_suspend() noexcept;
    suspend_always final_suspend() noexcept;
    void unhandled_exception() noexcept;
  };
};

template <typename _AwrT> auto SyncAwait(_AwrT &&A) {
  if (!A.await_ready()) {
    auto AwaitAsync = [&]() -> Task {
      try { (void)(co_await A); } catch (...) {} // { dg-error {coroutines require a traits template; cannot find 'std::coroutine_traits'} }
    };
    Task t = AwaitAsync();
  }
  return A.await_resume();
}

void f() {
  suspend_always test;
  SyncAwait(test);
}
