// { dg-additional-options "-O -Wunused-function" }

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
        std::suspend_never initial_suspend() const noexcept { return {}; }
        std::suspend_never final_suspend() const noexcept { return {}; }
        void return_void() const noexcept {}
        void unhandled_exception() const noexcept {}
    };
};

// This checks that the attribute is passed on to the outlined coroutine
// functions (so that there should be no diagnostic).
[[maybe_unused]] static dummy foo()
{ 
    co_return;
}
