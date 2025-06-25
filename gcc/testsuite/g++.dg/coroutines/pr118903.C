// { dg-additional-options "-fsyntax-only" }

#include <coroutine>

struct awaitable {
    constexpr bool await_ready() {
        return true;
    }
    void await_suspend(std::coroutine_handle<void>) {

    }
    constexpr int await_resume() {
        return 42;
    }
};

struct super_simple_coroutine {
    struct promise_type {
        constexpr auto initial_suspend() {
            return std::suspend_never();
        }
        constexpr auto final_suspend() const noexcept {
            return std::suspend_never();
        }
        constexpr void unhandled_exception() {
            // do nothing
        }
        constexpr auto get_return_object() {
            return super_simple_coroutine{};
        }
        constexpr void return_void() {
        }
    };
};

auto fib (float f) -> super_simple_coroutine {
    // if `co_await` is part of BodyStatement of a function
    // it makes it coroutine
    constexpr int x = co_await awaitable{}; // { dg-error {'co_await awaitable..' is not a constant expression} }
}
