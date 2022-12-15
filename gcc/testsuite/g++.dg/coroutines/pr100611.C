// { dg-do run }
/*
  Test that instances created in capture clauses within co_await statements do not
  get 'promoted'. This would lead to the members destructor getting called more
  than once.

  Correct output should look like:
  Foo(23) 0xf042d8
  Foo(const& 23) 0xf042ec
  ~Foo(23) 0xf042ec
  After co_await
  ~Foo(23) 0xf042d8
*/
#include <coroutine>
#include <iostream>

static unsigned int struct_Foo_destructor_counter = 0;
static bool lambda_was_executed = false;

class Task {
public:
  struct promise_type {
    Task get_return_object() {
      return {std::coroutine_handle<promise_type>::from_promise(*this)};
    }

    std::suspend_never initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    void return_void() {}
  };

  ~Task() {
    if (handle_) {
      handle_.destroy();
    }
  }

  bool await_ready() { return false; }
  bool await_suspend(std::coroutine_handle<>) { return false; }
  bool await_resume() { return false; }

private:
  Task(std::coroutine_handle<promise_type> handle) : handle_(handle) {}

  std::coroutine_handle<promise_type> handle_;
};

class Foo {
public:
  Foo(int id) : id_(id) {
    std::cout << "Foo(" << id_ << ") " << (void*)this << std::endl;
  }

  Foo(Foo const& other) : id_(other.id_) {
    std::cout << "Foo(const& " << id_ << ") " << (void*)this << std::endl;
  }

  Foo(Foo&& other) : id_(other.id_) {
    std::cout << "Foo(&& " << id_ << ") " << (void*)this << std::endl;
  }

  ~Foo() {
    std::cout << "~Foo(" << id_ << ") " << (void*)this << std::endl;
    struct_Foo_destructor_counter++;

    if (struct_Foo_destructor_counter > 2){
      std::cout << "Foo was destroyed more than two times!\n";
      __builtin_abort();
    }
    }

private:
  int id_;
};

Task test() {
  Foo foo(23);

  co_await [foo]() -> Task { // A copy of foo is captured. This copy must not get 'promoted'.
    co_return;
  }();

  std::cout << "After co_await\n";
  if (struct_Foo_destructor_counter == 0){
    std::cout << "The captured copy of foo was not destroyed after the co_await statement!\n";
    __builtin_abort();
  }
}

int main() {
  test();
  return 0;
}
