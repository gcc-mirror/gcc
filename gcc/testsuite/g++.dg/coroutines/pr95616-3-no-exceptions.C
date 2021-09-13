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
  ~finalSuspendAwaiter() noexcept(false) { }
  bool await_ready() const noexcept(false) { return false; }
  void await_suspend(std::coroutine_handle<>) const noexcept(false) { }
  int await_resume() const noexcept(false) { return x; }  // NOTE: not declared noexcept
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
  
  finalSuspendObj final_suspend() noexcept { return {3}; }

  void return_void() noexcept {}
  void unhandled_exception() noexcept {}
  };
};

// OK when exceptions are disabled
task f() {  
  co_return;
}
