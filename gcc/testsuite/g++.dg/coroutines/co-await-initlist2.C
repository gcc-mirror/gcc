// PR c++/109227
// { dg-do compile { target c++20 } }

#include <initializer_list>
#include <coroutine>

struct awaitable {
  bool await_ready();
  void await_suspend(std::coroutine_handle<> h);
  void await_resume();
};
awaitable async_new_session(int capabilities);
struct aa { ~aa(); };
aa f(void);
struct Capabilities {
  Capabilities(std::initializer_list<aa> __l);
};
int g(Capabilities);
struct task {
  struct promise_type {
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void unhandled_exception();
    task get_return_object() noexcept;
  };
};
task make() {
  co_await async_new_session(g({aa()}));
}
