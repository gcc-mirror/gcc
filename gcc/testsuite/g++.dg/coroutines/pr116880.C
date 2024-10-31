#include <coroutine>

struct promise_type;
using handle_type = std::coroutine_handle<promise_type>;

struct Co {
    handle_type handle;
    using promise_type = ::promise_type;

    explicit Co(handle_type handle) : handle(handle) {}

    bool await_ready() { return false; }
    std::coroutine_handle<> await_suspend(handle_type handle);
    void await_resume() {}
};

struct Done {};

struct promise_type {
    Co get_return_object();

    std::suspend_always initial_suspend() { return {}; };
    std::suspend_always final_suspend() noexcept { return {}; };
    void return_value(Done) {}
    void return_value(Co&&);
    void unhandled_exception() { throw; };
    Co&& await_transform(Co&& co) { return static_cast<Co&&>(co); }
};

Co tryToRun();

Co init()
{
    co_await tryToRun();
    co_return Done{};
}
