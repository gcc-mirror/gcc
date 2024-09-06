// { dg-do run }
// { dg-additional-options "-std=c++20" }

#include <coroutine>
#ifdef OUTPUT
#include <iostream>
#endif

struct Promise;

struct Handle : std::coroutine_handle<Promise> {
    using promise_type = Promise;
};

struct Promise {
    Handle get_return_object() noexcept {
        return {Handle::from_promise(*this)};
    }
    std::suspend_never initial_suspend() const noexcept { return {}; }
    std::suspend_never final_suspend() const noexcept { return {}; }
    void return_void() const noexcept {}
    void unhandled_exception() const noexcept {}
};

struct Awaiter {
    auto await_ready() const noexcept {
        return false;
    }

    auto await_suspend(auto self) const noexcept {
        self.resume();
        return false;
    }

    int await_resume() const noexcept { return 42; }
};

bool OK = false;

Handle Coro() {
#ifdef OUTPUT
    std::cout << "1\n";
    std::cout << co_await Awaiter{} << '\n';
#else
    (void) co_await Awaiter{};
#endif
#ifdef OUTPUT
    std::cout << "2\n";
#endif
    co_await std::suspend_always{};
#ifdef OUTPUT
    std::cout << "3\n";
#endif
    OK = true;
    co_return;
}

int main() {
    Coro();

    if (!OK)
      __builtin_abort ();
    return 0;
}
