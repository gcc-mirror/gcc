// { dg-do run { target c++17 } }
//
// Check if default return_void is insert at correct position.
#include <cassert>
#include "../coro.h"

class resumable {
public:
  class promise_type;
  using coro_handle = std::coroutine_handle<promise_type>;
  resumable(coro_handle handle) : handle_(handle) { assert(handle); }
  resumable(resumable&) = delete;
  resumable(resumable&&) = delete;
  bool resume() {
    if (!handle_.done())
      handle_.resume();
    return !handle_.done();
  }
  int recent_val();
  ~resumable() { handle_.destroy(); }
private:
  coro_handle handle_;
};

class resumable::promise_type {
public:
  friend class resumable;
  using coro_handle = std::coroutine_handle<promise_type>;
  auto get_return_object() { return coro_handle::from_promise(*this); }
  auto initial_suspend() { return std::suspend_always(); }
  auto final_suspend() { return std::suspend_always(); }
  void return_void() { value_ = -1; }
  void unhandled_exception() {}
private:
  int value_ = 0;
};

int resumable::recent_val() {return handle_.promise().value_;}

resumable foo(int n){
  co_await std::suspend_always();
  throw 1;
}

int bar (int n) {
  resumable res = foo(n);
  while(res.resume());
  return res.recent_val();
}

int main() {
  int res = bar(3);
  assert(res == 0);
  return 0;
}
