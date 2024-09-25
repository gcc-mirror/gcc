// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
/*
  Test that instances created in capture clauses within co_await statements do not get
  'promoted'. This would lead to their members destructors getting called more
  than once.

  Correct output should look like:
  START TASK
  Foo() 0x4f9320
  IN LAMBDA
  ~Foo() 0x4f9320
  TASK RETURN
*/
#include <coroutine>
#include <exception>
#include <iostream>
#include <utility>

static unsigned int struct_Foo_destructor_counter = 0;
static bool lambda_was_executed = false;

class Task {
public:
  struct promise_type {
    struct final_awaitable {
      bool await_ready() noexcept { return false; }
      auto await_suspend(std::coroutine_handle<promise_type> coro) noexcept {
        return coro.promise().continuation;
      }
      void await_resume() noexcept {}
    };
    Task get_return_object() {
      return Task(std::coroutine_handle<promise_type>::from_promise(*this));
    }
    std::suspend_always initial_suspend() { return {}; }
    final_awaitable final_suspend() noexcept { return {}; }
    void unhandled_exception() { std::terminate(); }
    void return_void() {}

    std::coroutine_handle<void> continuation = std::noop_coroutine();
  };

  Task(Task const&) = delete;
  Task(Task&& other) noexcept
      : handle_(std::exchange(other.handle_, nullptr)) {}
  Task& operator=(Task const&) = delete;
  Task& operator=(Task&& other) noexcept {
    handle_ = std::exchange(other.handle_, nullptr);
    return *this;
  }
  ~Task() {
    if (handle_) {
      handle_.destroy();
    }
  }

  bool await_ready() const { return false; }
  auto await_suspend(std::coroutine_handle<void> continuation) {
    handle_.promise().continuation = continuation;
    return handle_;
  }
  void await_resume() {}

private:
  explicit Task(std::coroutine_handle<promise_type> handle) : handle_(handle) {}

  std::coroutine_handle<promise_type> handle_;
};

struct RunTask {
  struct promise_type {
    RunTask get_return_object() { return {}; }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() { std::terminate(); }
  };
};

struct Foo {
  Foo() {
    std::cout << "Foo() " << (void *)this << std::endl;
  }

  ~Foo() {
    std::cout << "~Foo() " << (void *)this << std::endl;
    struct_Foo_destructor_counter++;

    if (struct_Foo_destructor_counter > 1 || !lambda_was_executed) {
      std::cout << "The destructor of Foo was called more than once or too early!\n";
      __builtin_abort();
    }
  }

  Foo(Foo&&) = delete;
  Foo(Foo const&) = delete;
  Foo& operator=(Foo&&) = delete;
  Foo& operator=(Foo const&) = delete;
};

Task DoAsync() {
  std::cout << "START TASK\n";
  co_await [foo = Foo{}]() -> Task { // foo is constructed inplace, no copy/move is performed.
                                     // foo itself must not get 'promoted'.
    std::cout << "IN LAMBDA\n";
    lambda_was_executed = true;
    co_return;
  }();
  // After the co_await statement the temporary lambda and foo
  // must now have been destroyed
  if (struct_Foo_destructor_counter == 0){
    std::cout << "foo was not destroyed after the co_await statement!\n";
    __builtin_abort();
  }
  std::cout << "TASK RETURN\n";
  co_return;
}

RunTask Main() { co_await DoAsync(); }

int main() {
  Main();
  return 0;
}
