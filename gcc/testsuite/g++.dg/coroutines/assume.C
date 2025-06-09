// { dg-additional-options "-fsyntax-only -Wattributes" }

#include <coroutine>

struct awaitable {
    awaitable (int n) : delay{n} {}

    constexpr bool await_ready () const noexcept { return false; }
    auto await_suspend (std::coroutine_handle<> h) const {
        __builtin_abort ();
        return false;
    }
    int await_resume() const noexcept {
        return delay;
    }

    int delay;
};

struct Task {
    struct promise_type {
        promise_type() = default;
        Task get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        void unhandled_exception() {}
        void return_void () {}
        awaitable yield_value (int v) { return {v}; }
    };
};

int h () { return 5; }

Task foo() noexcept {
    int x = 5;
    [[assume (x == 5)]];
    [[assume (co_await awaitable{10})]]; // { dg-warning {assumption ignored because it contains an await-expression} }
    [[assume ((h(),co_await awaitable{11}))]]; // { dg-warning {assumption ignored because it contains an await-expression} }
    co_return;
}
