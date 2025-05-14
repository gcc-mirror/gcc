// PR c++/118874
// { dg-do compile }
// { dg-additional-options "-std=c++20" }

#include <coroutine>

struct B {
  bool await_ready () const noexcept;
  void await_suspend (std::coroutine_handle<> h) const noexcept;
  void await_resume () const noexcept;
};

struct C {
  struct promise_type {
    const char *value;
    std::suspend_never initial_suspend ();
    std::suspend_always final_suspend () noexcept;
    void return_value (const char *v);
    void unhandled_exception ();
    C get_return_object () { return C{this}; }
  };
  promise_type *p;
  explicit C (promise_type *p) : p(p) {}
  const char *get ();
};

C
bar (bool x)
{
  if (x)
    co_await B{};
  co_return "foobar";
}
