// { dg-additional-options "-fsyntax-only" }
// PR116775
#include <coroutine>
#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF(...)
#else
#include <cstdio>
#  define PRINT(X) puts(X)
#  define PRINTF(...) printf(__VA_ARGS__)
#endif

struct awaitable {
    awaitable(int n) : delay{n} {}

    constexpr bool await_ready() const noexcept { return false; }
    auto await_suspend(std::coroutine_handle<> h) const {
        __builtin_trap ();
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

Task foo() noexcept {
    if (__builtin_constant_p (true ? 1 : co_await awaitable{10}))
      PRINT ("const OK");
    else
      {
        PRINT ("failed : should be const");
        __builtin_abort ();
      }
    if (__builtin_constant_p (false ? 1 : co_await awaitable{15}))
      {
        PRINT ("failed : should not be const");
        __builtin_abort ();
      }
    else
      PRINT ("not const, OK");
    if (__builtin_constant_p (5 == (co_yield 42)))
      {
        PRINT ("failed : should not be const");
        __builtin_abort ();
      }
    else
      PRINT ("not const, OK");
    co_return;
}

//call foo
int main() {
    foo();
}
