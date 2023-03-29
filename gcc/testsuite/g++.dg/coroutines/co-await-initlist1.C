// PR c++/103871
// { dg-do compile { target c++20 } }

#include <coroutine>
#include <initializer_list>

struct my_coro {
  struct promise_type {
    my_coro get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void unhandled_exception();
  };
};

std::suspend_never inner(std::initializer_list<int>);

my_coro doesnt_work()
{
  co_await inner({ 1,2,3 });
}
