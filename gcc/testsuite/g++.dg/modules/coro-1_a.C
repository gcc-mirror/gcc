// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules -fdump-lang-module" }
// { dg-module-cmi M }

module;
#include <coroutine>
export module M;

struct simple_promise;
struct simple_coroutine : std::coroutine_handle<simple_promise> {
  using promise_type = ::simple_promise;
};
struct simple_promise {
  simple_coroutine get_return_object() { return { simple_coroutine::from_promise(*this) }; }
  std::suspend_always initial_suspend() noexcept { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void return_void() {}
  void unhandled_exception() {}
};
export simple_coroutine coroutine() {
  co_return;
}
export inline simple_coroutine inline_coroutine() {
  co_return;
}
export template <typename T> simple_coroutine template_coroutine() {
  co_return;
}

// The actor and destroy functions should have definitions streamed only for inline coroutines
// { dg-final { scan-lang-dump-not {Writing definition function_decl:'::coroutine.actor'} module } }
// { dg-final { scan-lang-dump-not {Writing definition function_decl:'::coroutine.destroy'} module } }
// { dg-final { scan-lang-dump {Writing definition function_decl:'::inline_coroutine.actor'} module } }
// { dg-final { scan-lang-dump {Writing definition function_decl:'::inline_coroutine.destroy'} module } }
