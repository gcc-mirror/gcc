// { dg-do run }
// { dg-additional-options "-fno-exceptions" }
                                                       
#include <coroutine>

bool g_too_early = true;
std::coroutine_handle<> g_handle;

struct Awaiter {
    Awaiter() {}
    ~Awaiter() {
        if (g_too_early) {
            __builtin_abort ();
        }
    }

    bool await_ready() { return false; }
    void await_suspend(std::coroutine_handle<> handle) {
        g_handle = handle;
    }

    void await_resume() {}
};

struct SuspendNever {
    bool await_ready() noexcept { return true; }
    void await_suspend(std::coroutine_handle<>) noexcept {}
    void await_resume() noexcept {}
};

struct Coroutine {
    struct promise_type {
        Coroutine get_return_object() { return {}; }
        SuspendNever initial_suspend() { return {}; }
        SuspendNever final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}

        Awaiter&& await_transform(Awaiter&& u) {
            return static_cast<Awaiter&&>(u);
        }
    };
};

Coroutine foo() {
    co_await Awaiter{};
}

int main() {
    foo();
    g_too_early = false;
    g_handle.destroy();
}
