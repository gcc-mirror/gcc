// PR c++/120243
// { dg-do run }

#include <coroutine>

struct coro {
     struct promise_type {
        promise_type() = default;

        std::suspend_always initial_suspend() const noexcept { return {}; }
        std::suspend_always final_suspend() const noexcept { return {}; }

        void unhandled_exception() { throw; }

        coro get_return_object() { return {std::coroutine_handle<promise_type>::from_promise(*this)}; }
        void return_void() {}
    };

    std::coroutine_handle<promise_type> h;
};

int main() {
    auto c = []() -> coro {
        throw "hello";
        __builtin_abort();
        co_return;
    };
    try {
        c().h.resume();
    }
    catch(...) {
        __builtin_printf("Caught!\n");
    }
}
