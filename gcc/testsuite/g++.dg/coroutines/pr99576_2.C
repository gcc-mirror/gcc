// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
/*
  Test that members of temporary awaitables in co_await statements do not get
  'promoted'. This would lead to the members destructor getting called more
  than once.

  Correct output should look like:
  A 0x4f82d6
  ~A 0x4f82d6
*/
#include <coroutine>
#include <iostream>


static unsigned int struct_A_destructor_counter = 0;

struct task : std::coroutine_handle<> {
  struct promise_type;
};

struct task::promise_type {
  task get_return_object() {
    return {std::coroutine_handle<promise_type>::from_promise(*this)};
  }
  std::suspend_always initial_suspend() { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void unhandled_exception() { std::terminate(); }
  void return_void() {}
};

struct A {
  void log(const char *str) { std::cout << str << " " << (void *)this << std::endl; }

  A() { log(__func__); }

  ~A() {
    log(__func__);
    struct_A_destructor_counter++;

    if (struct_A_destructor_counter > 1) {
      std::cout << "The destructor of A was called more than once!\n";
      __builtin_abort();
    }
  }

  A(A&&) = delete;
  A(A const&) = delete;
  A& operator=(A&&) = delete;
  A& operator=(A const&) = delete;
};

struct Awaitable {
  A a{}; // <- This member must NOT get 'promoted'
  bool await_ready() { return false; }
  void await_suspend(std::coroutine_handle<> handle) {}
  void await_resume() {}
};

task coroutine() {
  co_await Awaitable{}; // <- This temporary must get 'promoted'
}

int main() {

  auto task = coroutine();
  while (!task.done()) {
    task();
  }
  task.destroy();

  return 0;
}
