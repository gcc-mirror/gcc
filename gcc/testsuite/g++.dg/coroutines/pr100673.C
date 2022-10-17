//  { dg-additional-options "-fsyntax-only -w" }

// Diagnose bad coroutine awatiable type.

#include<coroutine>

struct coro{
  struct not_a_template{};
  using promise_type = coro;
  static constexpr std::suspend_always initial_suspend()noexcept{ return {}; }
  constexpr bool await_ready()noexcept{ return false; }
  constexpr not_a_template await_suspend(std::coroutine_handle<>)noexcept{ return{}; }
  constexpr void await_resume()noexcept{}
  static coro body()
  {
    co_await coro{}; // { dg-error {'await_suspend' must return 'void', 'bool' or a coroutine handle} }
  }
};
