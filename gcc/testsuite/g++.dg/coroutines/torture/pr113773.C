//  { dg-do run }
#include <coroutine>
#ifdef OUTPUT
#include <iostream>
#endif

struct result {
  operator int() {
    throw 42;
  }
};

static int p_dtor_count = 0;
class promise {
public:
  result get_return_object() { return {}; }
  std::suspend_never initial_suspend() {
#ifdef OUTPUT
    std::cout << "initial suspend" << std::endl;
#endif
    return {};
  }
  void unhandled_exception() {
#ifdef OUTPUT
    std::cout << "unhandled exception" << std::endl;
#endif
  }
  std::suspend_never final_suspend() noexcept {
#ifdef OUTPUT
    std::cout << "final suspend" << std::endl;
#endif
    return {};
  }
  void return_void() {}
  ~promise() {
    p_dtor_count++;
#ifdef OUTPUT
   std::cout << "~promise()" << std::endl;
#endif
  }
};

template <class... Args>
struct std::coroutine_traits<int, Args...> {
  using promise_type = promise;
};

int f() {
  co_return;
}

int main() {
  try {
    f();
  }
  catch (int i) {
    if (i != 42)
      __builtin_abort ();
#ifdef OUTPUT
    std::cout << "caught 42" << std::endl;
#endif
  }
  if (p_dtor_count != 1)
    __builtin_abort ();
  return 0;
}
