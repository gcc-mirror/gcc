#include "coro.h"

struct promise;

struct future
{
    using promise_type = promise;
};

struct promise
{
  template<typename Class>
  promise(Class &,int) { static_assert(!std::is_pointer<Class>::value, ""); }

  coro::suspend_never initial_suspend() { return {}; }
  coro::suspend_never final_suspend() { return {}; }

  future get_return_object() { return {}; }

  void return_value(int) {}
  void unhandled_exception() {}
};

struct bar
{
  future foo(int param) { co_return 0; }
};