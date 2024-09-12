//  { dg-do run { target c++17 } }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

class resumable {
public:
  struct promise_type;
  using coro_handle = std::coroutine_handle<promise_type>;
  resumable(coro_handle handle) : handle_(handle) { }
  resumable(resumable&) = delete;
  resumable(resumable&&) = delete;
  ~resumable() { handle_.destroy(); }
  coro_handle handle_;
};

struct resumable::promise_type {
  using coro_handle = std::coroutine_handle<promise_type>;
  int used;
  auto get_return_object() {
    return coro_handle::from_promise(*this);
  }
  auto initial_suspend() { return std::suspend_never(); }
  auto final_suspend() noexcept { return std::suspend_always(); }
  void return_value(int x) {used = x;}
  void unhandled_exception() {}

  struct TestAwaiter {
    int recent_test;
    TestAwaiter(int test) : recent_test{test} {}
    bool await_ready() { return false; }
    void await_suspend(std::coroutine_handle<promise_type>) {}
    int await_resume() {
      return recent_test;
    }
    auto operator co_await() {
      return *this;
    }
  };

  struct TestAwaiterCH :TestAwaiter { 
    TestAwaiterCH(int test) : TestAwaiter(test) {};
  };

  struct TestAwaiterCHCH :TestAwaiterCH {
    TestAwaiterCHCH(int test) : TestAwaiterCH(test) {};

    resumable foo(){
    int x = co_await *this;
    co_return x;
    }
  };
};

struct TestP {
 resumable::promise_type::TestAwaiterCHCH  tp = resumable::promise_type::TestAwaiterCHCH(6);
};

resumable foo1(int t){
  int x = co_await resumable::promise_type::TestAwaiterCH(t);
  co_return x;
}

resumable foo2(){
  struct TestP  TP;
  int x = co_await TP.tp;
  co_return x;
}

resumable foo3(){
  int x = co_await TestP{}.tp;
  co_return x;
}

int main(){
  auto t = resumable::promise_type::TestAwaiterCHCH(4);
  resumable res = t.foo();
  while (!res.handle_.done())
    res.handle_.resume();
  if (res.handle_.promise().used != 4)
    abort();

  resumable res1 = foo1(5);
  while (!res1.handle_.done())
    res1.handle_.resume();
  if (res1.handle_.promise().used != 5)
    abort();

  resumable res2 = foo2();
  while (!res2.handle_.done())
    res2.handle_.resume();
  if (res2.handle_.promise().used != 6)
    abort();
  
  resumable res3 = foo2();
  while (!res3.handle_.done())
    res3.handle_.resume();
  if (res3.handle_.promise().used != 6)
    abort();
}
