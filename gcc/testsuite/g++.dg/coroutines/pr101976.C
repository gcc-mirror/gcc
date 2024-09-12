// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

/*
  Test that members of temporary instances in co_await statements do not get
  'promoted'. This would lead to the members destructor getting called more
  than once.

  Correct output should look like:
  Before co_await
  nontrivial_move() 0x6ec2e1
  nontrivial_move(nontrivial_move&&) 0x6ed320
  In subtask
  ~nontrivial_move() 0x6ed320
  ~nontrivial_move() 0x6ec2e1
  After co_await
*/
#include <coroutine>
#include <iostream>

static unsigned int struct_nontrivial_move_destructor_counter = 0;

struct task {
  struct promise_type {
    task get_return_object() {
      return {std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    void return_void() {}
  };

  bool await_ready() { return true; }
  void await_suspend(std::coroutine_handle<>) {}
  void await_resume() {}

  std::coroutine_handle<promise_type> m_handle;
};

struct nontrivial_move {
  nontrivial_move() {
    std::cout << "nontrivial_move() " << (void *)this << std::endl;
  }
  nontrivial_move(nontrivial_move&&) {
    std::cout << "nontrivial_move(nontrivial_move&&) " << (void *)this
              << std::endl;
  }
  ~nontrivial_move() {
    std::cout << "~nontrivial_move() " << (void *)this << std::endl;
    struct_nontrivial_move_destructor_counter++;
    if (struct_nontrivial_move_destructor_counter > 2){
      std::cerr << "The destructor of nontrivial_move was called more than two times!\n";
      __builtin_abort();
    }
  }

  char buf[128]{}; // Example why the move could be non trivial
};

struct wrapper {
  nontrivial_move member;
};

task subtask(wrapper /* unused */) {
  std::cout << "In subtask\n";
  co_return;
}

task main_task() {
  std::cout << "Before co_await\n";
  co_await subtask({}); // wrapper must get 'promoted', but not its member
  std::cout << "After co_await\n";
}

int main() {
  main_task();
  return 0;
}
