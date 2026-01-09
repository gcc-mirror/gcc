// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
import std;  // not exported

export template <typename T> void structured_binding() {
  std::tuple<T> t;
  auto [x] = t;
}

export template <typename T> void operator_new() {
  // PR c++/101140
  T x;
  new (&x) T;
}

export template <typename T> void use_typeid() {
  const auto& id = typeid(T);
}

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
template <typename T> simple_coroutine coroutine_impl() {
  co_return;
}
export template <typename T> void coroutine() {
  simple_coroutine sc = coroutine_impl<T>();
  sc.resume();
  sc.destroy();
}
