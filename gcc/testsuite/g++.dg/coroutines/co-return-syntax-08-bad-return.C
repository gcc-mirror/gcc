//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

struct Coro {
  struct promise_type;
  using handle_type = coro::coroutine_handle<Coro::promise_type>;
  handle_type handle;
  Coro () : handle(0) {}
  Coro (handle_type _handle) : handle(_handle) {}
  Coro (Coro &&s) : handle(s.handle) { s.handle = nullptr; }
  Coro &operator = (Coro &&s) {
	handle = s.handle;
	s.handle = nullptr;
	return *this;
  }
  Coro (const Coro &) = delete;
  ~Coro() {
    if ( handle )
      handle.destroy();
  }
  struct promise_type {
  promise_type() {}
  ~promise_type() {}
  Coro get_return_object () { return Coro (handle_type::from_promise (*this)); }
  auto initial_suspend () { return coro::suspend_always{}; }
  auto final_suspend () { return coro::suspend_always{}; }
  void return_void () { }
   void unhandled_exception() { }
  };
};

extern int x;

// Diagnose disallowed "return" in coroutine.
Coro
bar () // { dg-error {a 'return' statement is not allowed} }
{
  if (x)
    return Coro(); 
  else
    co_return;
}
