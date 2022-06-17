// { dg-additional-options "-fsyntax-only" }
namespace std {
template <class T, class = void>
struct traits_sfinae_base {};

template <class Ret, class... Args>
struct coroutine_traits : public traits_sfinae_base<Ret> {};
}

template<typename Promise> struct coro {};
template <typename Promise, typename... Ps>
struct std::coroutine_traits<coro<Promise>, Ps...> {
  using promise_type = Promise;
};

struct awaitable {
  bool await_ready() noexcept;
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept;
} a;

struct suspend_always {
  bool await_ready() noexcept { return false; }
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept {}
};

namespace std {
template <class PromiseType = void>
struct coroutine_handle {};
}

struct bad_promise_6 {
  coro<bad_promise_6> get_return_object();
  suspend_always initial_suspend();
  suspend_always final_suspend() noexcept;
  void unhandled_exception();
  void return_void();
  void return_value(int) const;
  void return_value(int);
};

coro<bad_promise_6>
bad_implicit_return() // { dg-error {.aka 'bad_promise_6'. declares both 'return_value' and 'return_void'} }
{
  co_await a;
}
