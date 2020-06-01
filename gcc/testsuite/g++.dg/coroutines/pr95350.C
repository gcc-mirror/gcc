#if __has_include(<coroutine>)
#include <coroutine>
#else
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

struct move_only {
    move_only();
    move_only(const move_only&) = delete;
    move_only(move_only&) = delete;
    move_only(move_only&&) = default;
};

task f(move_only x) {
    co_return;
}
