
#include <coroutine>

class Task {
 public:
  struct promise_type {
    Task get_return_object() { return Task{}; }
    std::suspend_always initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    void return_void() {}
  };

  bool await_ready() const { return false; }
  void await_suspend(std::coroutine_handle<void> continuation) {}
  void await_resume() {}
};

class NonMoveableTask {
 public:
  NonMoveableTask() = default;
  NonMoveableTask(const NonMoveableTask&) = delete;
  NonMoveableTask(NonMoveableTask&&) = delete;

  NonMoveableTask& operator=(const NonMoveableTask&) = delete;
  NonMoveableTask& operator=(NonMoveableTask&& other) = delete;

  bool await_ready() const { return false; }
  void await_suspend(std::coroutine_handle<void>) {}
  void await_resume() {}
};

Task Foo(NonMoveableTask* task) { co_await* task; }

int main() {}
