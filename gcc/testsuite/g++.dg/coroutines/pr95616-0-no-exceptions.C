//  { dg-additional-options "-fsyntax-only -fno-exceptions" }

#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std {
  using namespace std::experimental;
}
#endif

class promise;

struct finalSuspendAwaiter {
  int x;
  finalSuspendAwaiter () : x(0) { }
  finalSuspendAwaiter (int _x) : x(_x) { }
  ~finalSuspendAwaiter() noexcept(true) { }
  bool await_ready() const noexcept(true) { return false; }
  void await_suspend(std::coroutine_handle<>) const noexcept(true) { }
  int await_resume() const noexcept(true) { return x; }
};

struct finalSuspendObj {
  int x;
  finalSuspendObj () : x(0) { }
  finalSuspendObj (int _x) : x(_x) { }
  ~finalSuspendObj () noexcept(true) {} 

  finalSuspendAwaiter operator co_await() const & noexcept(true) { 
    return {x};
  }
};

struct task {
  struct promise_type {
  task get_return_object() noexcept { return {}; }
  std::suspend_never initial_suspend() noexcept { return {}; }
  
  finalSuspendObj final_suspend() { return {3}; } // NOTE: not declared noexcept

  void return_void() noexcept {}
  void unhandled_exception() noexcept {}
  };
};

// OK when exceptions are disabled
task f() {  
  co_return;
}
