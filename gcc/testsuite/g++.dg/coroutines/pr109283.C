// PR 109283.
// This used to ICE from a check set too widely.
#include <coroutine>

struct foo
{ ~foo(); };

struct task
{
   struct promise_type
   {
       std::suspend_never initial_suspend();
       std::suspend_never final_suspend() noexcept;
       std::suspend_never yield_value(foo);
       void return_void();
       void unhandled_exception(); 
       task get_return_object();
   };
};

task source(int b) {
   co_yield b ? foo{} : foo{};
}
