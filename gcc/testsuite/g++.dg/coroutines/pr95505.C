#if __has_include (<coroutine>)
#include <coroutine>
using namespace std;
#elif defined (__clang__) && __has_include (<experimental/coroutine>)
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif

struct dummy
{
    struct promise_type
    {
        dummy get_return_object() const noexcept { return {}; }
        static dummy get_return_object_on_allocation_failure() noexcept { return {}; }
        std::suspend_always initial_suspend() const noexcept { return {}; }
        std::suspend_never final_suspend() const noexcept { return {}; }
        void return_void() const noexcept {}
        void unhandled_exception() const noexcept {}
    };
};

dummy foo() // { dg-error {dummy::promise_type::get_return_object_on_allocation_failure.*but 'std::nothrow' cannot be found} }
{
    co_return;
}

