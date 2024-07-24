#include <coroutine>

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

Handle Coro() {
    struct Awaiter : std::suspend_never {
        int await_resume() { return 0; }
    };

    [] (int x = co_await Awaiter{}){}; // { dg-error ".co_await. cannot be used in default arguments" }
    co_return;
}

int main() {
    Coro();

    return 0;
}
