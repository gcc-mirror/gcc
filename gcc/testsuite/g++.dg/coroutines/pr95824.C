#include <coroutine>

struct task {
  struct promise_type {
    auto initial_suspend() noexcept { return std::suspend_always{}; }
    auto final_suspend() noexcept { return std::suspend_always{}; }
    void return_void() {}
    task get_return_object() { return task{}; }
    void unhandled_exception() noexcept {}
  };

  ~task() noexcept {}

  bool await_ready() const noexcept { return false; }
  void await_suspend(std::coroutine_handle<>) noexcept {}
  void await_resume() noexcept {}
};

struct base
{
    virtual ~base() = default;
};

class exception : public virtual base
{};

struct factory
{
    virtual ~factory() = default;
    virtual int makeId() const;
};

task g(int);

task f(factory& factory) {
    co_await g(factory.makeId());
}
