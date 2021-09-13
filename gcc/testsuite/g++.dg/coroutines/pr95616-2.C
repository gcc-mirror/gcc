//  { dg-additional-options "-fsyntax-only" }

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
  int await_resume() const noexcept(false) { return x; }
};

struct finalSuspendObj {
  int x;
  finalSuspendObj () : x(0) { }
  finalSuspendObj (int _x) : x(_x) { }
  ~finalSuspendObj () noexcept(false) {} 
  // { dg-error {the expression 'finalSuspendObj::~finalSuspendObj' is required to be non-throwing} "" { target *-*-* } .-1 }

  finalSuspendAwaiter operator co_await() const & noexcept(true) { 
    return {x};
  }
};

struct task {
  struct promise_type {
  task get_return_object() noexcept { return {}; }
  std::suspend_never initial_suspend() noexcept { return {}; }
  
  finalSuspendObj final_suspend() noexcept { return {3}; } // NOTE: not declared noexcept

  void return_void() noexcept {}
  void unhandled_exception() noexcept {}
  };
};

// This should be ill-formed since final_suspend() is potentially throwing.
task f() {  
  co_return;
}
