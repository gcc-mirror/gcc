// { dg-do compile }
#include <coroutine>

struct must_check_result
{
    bool await_ready() { return false; }
    void await_suspend(std::coroutine_handle<>) {}
    [[nodiscard]] bool await_resume() { return {}; }
};

struct task {};

namespace std
{
    template<typename... Args>
    struct coroutine_traits<task, Args...>
    {
        struct promise_type
        {
            task get_return_object() { return {}; }
            suspend_always initial_suspend() noexcept { return {}; }
            suspend_always final_suspend() noexcept { return {}; }
            void return_void() {}
            void unhandled_exception() {}
        };
    };
}

task example()
{
    co_await must_check_result(); // { dg-warning "-Wunused-result" }
}
