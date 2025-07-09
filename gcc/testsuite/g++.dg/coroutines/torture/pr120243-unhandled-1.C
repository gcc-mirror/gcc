// PR c++/120243
// { dg-do run }

#include <coroutine>

struct coro {
     struct promise_type {
        promise_type() = default;

        std::suspend_never initial_suspend() const noexcept { return {}; }
        std::suspend_never final_suspend() const noexcept { return {}; }

        void unhandled_exception() { throw; }

        coro get_return_object() { return {}; }
        void return_void() {}

    };
};

int main() {
    auto c = []() -> coro {
        throw "hello";
        __builtin_abort();
        co_return;
    };
    try {
        c();
    }
    catch(...) {
        __builtin_printf("Caught!\n");
    }
}
