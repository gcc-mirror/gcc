// https://gcc.gnu.org/PR106973
// { dg-require-effective-target indirect_jumps }
#include <coroutine>
#include <setjmp.h>

struct generator;
struct generator_promise {
  generator get_return_object();
  std::suspend_always initial_suspend();
  std::suspend_always final_suspend() noexcept;
  std::suspend_always yield_value(int);
  void unhandled_exception();
};

struct generator {
  using promise_type = generator_promise;
};
jmp_buf foo_env;
generator foo() {
  setjmp(foo_env);
  co_yield 1;
}
