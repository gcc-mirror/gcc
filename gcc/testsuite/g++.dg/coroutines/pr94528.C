//  { dg-additional-options "-std=c++20 -fpreprocessed -w" }
namespace std {
inline namespace {
template <typename _Result, typename> struct coroutine_traits {
  using promise_type = _Result::promise_type;
};
template <typename = void> struct coroutine_handle;
template <> struct coroutine_handle<> { public: };
template <typename> struct coroutine_handle : coroutine_handle<> {};
struct suspend_always {
  bool await_ready();
  void await_suspend(coroutine_handle<>);
  void await_resume();
};
} // namespace
} // namespace std
namespace coro = std;
namespace cppcoro {
class task {
private:
  struct awaitable_base {
    coro::coroutine_handle<> m_coroutine;
    bool await_ready() const noexcept;
    void await_suspend(coro::coroutine_handle<> awaitingCoroutine) noexcept;
  };

public:
  auto operator co_await() const &noexcept {
    struct awaitable : awaitable_base {
      decltype(auto) await_resume() {}
    };
    return awaitable{m_coroutine};
  }

private:
  coro::coroutine_handle<> m_coroutine;
};
class shared_task;
class shared_task_promise_base {
  struct final_awaiter {
    bool await_ready() const noexcept;
    template <typename PROMISE>
    void await_suspend(coro::coroutine_handle<PROMISE> h) noexcept;
    void await_resume() noexcept;
  };

public:
  coro::suspend_always initial_suspend() noexcept;
  final_awaiter final_suspend() noexcept;
  void unhandled_exception() noexcept;
};
class shared_task_promise : public shared_task_promise_base {
public:
  shared_task get_return_object() noexcept;
  void return_void() noexcept;
};
class shared_task {
public:
  using promise_type = shared_task_promise;
};
auto make_shared_task(cppcoro::task awaitable) -> shared_task {
  co_return co_await static_cast<cppcoro::task &&>(awaitable);
}
} // namespace cppcoro
