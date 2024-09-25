// Override default options.
// { dg-options "-std=c++20 -fno-exceptions -Wall -Wextra" }

#include <coroutine>

struct SuspendNever {
    bool await_ready();
    void await_suspend(std::coroutine_handle<>);
    void await_resume();
};

struct Coroutine;

struct PromiseType {
    Coroutine get_return_object();
    SuspendNever initial_suspend();
    SuspendNever final_suspend();
#if __cpp_exceptions
    void unhandled_exception() { /*std::terminate();*/ };
#endif
    void return_void();
};

struct Coroutine {
    using promise_type = PromiseType;
};

Coroutine __async_test_input_basic() {
    co_return;
}
