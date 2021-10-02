#include <coroutine>
#include <string>

template<typename T>
struct Awaiter
{
    bool await_ready()                          const { return false; }    
    void await_suspend(std::coroutine_handle<>) const {}    
    T    await_resume()                         const { return T{}; }
};

struct ReturnObject
{
    struct promise_type
    {
        ReturnObject       get_return_object()        { return {}; }
        std::suspend_never initial_suspend() noexcept { return {}; }
        std::suspend_never final_suspend()   noexcept { return {}; }
        void               return_void()              {}
        void               unhandled_exception()      {}
    };
};

ReturnObject f()
{
    auto a1 = Awaiter<int>{};
    auto a2 = Awaiter<std::string>{};

    [[maybe_unused]] auto v1 = co_await a1; // ok
    [[maybe_unused]] std::string v2 = co_await a2; // error
}
