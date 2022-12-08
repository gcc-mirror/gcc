//  { dg-additional-options "-Wzero-as-null-pointer-constant -fsyntax-only" }

#include <coroutine>
 
struct task
{
    struct promise_type
    {
        task get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
    };
};
 
task resuming_on_new_thread(void)
{
    struct awaitable
    {
        bool await_ready() { return false; }
        void await_suspend(std::coroutine_handle<> h)         {         }
        void await_resume() {}
    };
    co_await awaitable{};
}
