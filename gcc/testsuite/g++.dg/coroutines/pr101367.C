// { dg-do run }

#include <coroutine>
using namespace std;
#include <cstdio>
#include <utility>
#include <string>

struct resource {
    template<typename Func>
    resource(Func fn) { fn(); /*std::printf("resource()\n"); */}
    ~resource() { /*std::printf("~resource()\n"); */}
    resource(resource&&) = delete;
};

template<typename T>
struct generator {
    struct promise_type {
        generator get_return_object() {
            return generator{coroutine_handle<promise_type>::from_promise(*this)};
        }

        void return_void() {}
        void unhandled_exception() {}
        suspend_always initial_suspend() { return {}; }
        suspend_always final_suspend() noexcept { return {}; }

        struct awaitable {
            resource& r;

            awaitable(resource&& r) : r(r) {}

            bool await_ready() noexcept { return false; }

            void await_suspend(coroutine_handle<> h) noexcept {
                //std::printf("awaitable::await_suspend()\n");
            }

            void await_resume() noexcept {
                //std::printf("awaitable::await_resume()\n");
            }
        };

        awaitable yield_value(resource&& r) {
            return awaitable{std::move(r)};
        }
    };

    generator(coroutine_handle<promise_type> coro) : coro(coro)
    {}

    generator(generator&& g) noexcept : coro(std::exchange(g.coro, {}))
    {}

    ~generator() {
        if (coro) { coro.destroy(); }
    }

    coroutine_handle<promise_type> coro;
};

generator<int> f() {
    std::string s;
    // if `s` isn't captured things work ok
    co_yield resource{[s]{}};
}

int main() {
    generator x = f();
    x.coro.resume();
    x.coro.resume();
}
