#include <coroutine>

struct Foo {
    ~Foo() noexcept(false); // true succeeds
    struct promise_type {
        Foo get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        void return_void() {}
        void unhandled_exception() {}
        std::suspend_always final_suspend() noexcept { return {}; }
    };
};

Foo bar() {
    co_return;
}
