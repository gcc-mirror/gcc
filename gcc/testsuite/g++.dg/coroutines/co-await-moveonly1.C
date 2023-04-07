// PR c++/105406
// { dg-do compile { target c++20 } }

#include <coroutine>
#include <exception>

// A move-only awaitable
class MoveOnlyAwaitable {
public:
    MoveOnlyAwaitable() = default;
    MoveOnlyAwaitable(MoveOnlyAwaitable &&) = default;
    MoveOnlyAwaitable &operator=(MoveOnlyAwaitable &&) = default;

    MoveOnlyAwaitable(const MoveOnlyAwaitable &) = delete;
    MoveOnlyAwaitable &operator=(const MoveOnlyAwaitable &) = delete;

    bool await_ready() const noexcept { return false; }
    void await_suspend(std::coroutine_handle<>) noexcept {}
    void await_resume() {}
};

struct task {
    struct promise_type {
        auto initial_suspend() const { return std::suspend_never{}; }
        auto final_suspend() const noexcept { return std::suspend_never(); }
        auto get_return_object() { return task{}; }
        void return_void() {}
        void unhandled_exception() {}

        template<typename T>
        T &&await_transform(T &&t) {
            return static_cast<T &&>(t);
        }


    };

    bool await_ready() const { return false; }
    void await_suspend(std::coroutine_handle<> awaiter) {}
    void await_resume() {}
};

task myCoroutine() {
    // GCC: OK
    // clang: OK
    {
        co_await MoveOnlyAwaitable();
    }
    // GCC: OK
    // clang: OK
    {
        auto moveonly = MoveOnlyAwaitable();
        co_await std::move(moveonly);
    }

    // GCC <= 11.2: OK
    // GCC 11.3:ERROR:  error: use of deleted function 'MoveOnlyAwaitable::MoveOnlyAwaitable(const MoveOnlyAwaitable&)
    // clang: OK
    {
        auto moveonly = MoveOnlyAwaitable();
        co_await moveonly;
    }
}
