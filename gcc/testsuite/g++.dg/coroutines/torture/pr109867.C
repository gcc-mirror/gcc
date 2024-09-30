// https://gcc.gnu.org/PR109867
// { dg-additional-options "-Werror=switch-default" }
#include <coroutine>

struct task
{
    struct promise_type
    {
        std::suspend_always initial_suspend();
        std::suspend_always final_suspend() noexcept;
        void unhandled_exception();
        task get_return_object();
        void return_value(int);
    };
};

int main()
{
    __attribute__((__unused__)) auto t = []() -> task
    {
        co_return 2;
    }();
}
