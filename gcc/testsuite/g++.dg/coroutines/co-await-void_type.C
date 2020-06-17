//  { dg-additional-options "-std=c++17 -fsyntax-only -w" }

#include <coroutine>

class resumable {
public:
  struct promise_type;
  using coro_handle = std::coroutine_handle<promise_type>;
  resumable(coro_handle handle) : handle_(handle) {}
  resumable(resumable&) = delete;
  resumable(resumable&&) = delete;
  bool resume() {
    if (not handle_.done())
      handle_.resume();
    return not handle_.done();
  }
  int recent_val();
  ~resumable() { handle_.destroy(); }
private:
  coro_handle handle_;
};

struct resumable::promise_type {
  int value_;

  using coro_handle = std::coroutine_handle<promise_type>;
  auto get_return_object() {
    return coro_handle::from_promise(*this);
  }
  auto initial_suspend() { return std::suspend_always(); }
  auto final_suspend() { return std::suspend_always(); }
  void yield_value(int v) { value_ = v; }
  void unhandled_exception() {}
};

int resumable::recent_val(){return handle_.promise().value_;}

resumable foo(int n){
  int x = 1;
  co_await std::suspend_always();
  int y = 2;
  co_yield n + x + y;  // { dg-error "awaitable type 'void' is not a structure" }
}

