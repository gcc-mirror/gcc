#if __has_include(<coroutine>)
#include <coroutine>
#elif defined (__clang__) && __has_include (<experimental/coroutine>)
#include <experimental/coroutine>
namespace std { using namespace experimental; }
#endif
#include <utility>

struct task {
    struct promise_type {
        task get_return_object();
        void return_void();
        void unhandled_exception();
        std::suspend_always initial_suspend() noexcept;
        std::suspend_always final_suspend() noexcept;
    };
};

struct wrapper {
    using promise_type = task::promise_type;
    wrapper(task&&);
};

wrapper f() {
    co_return;
}
