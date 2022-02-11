// { dg-additional-options " -O1 " }
#include <coroutine>

struct coroutine {
    struct promise_type {
        coroutine get_return_object() { return {}; }
        void return_void() {}
        void unhandled_exception() {}
        auto initial_suspend() noexcept { return std::suspend_never{}; }
        auto final_suspend() noexcept { return std::suspend_never{}; }
    };
};

struct data {
    constexpr int get() { return 5; }
};

struct test {
    data _data;

    void foo() {
        [this]() -> coroutine {
            _data.get();
            co_return;
        };
    }
};

int main() {}
