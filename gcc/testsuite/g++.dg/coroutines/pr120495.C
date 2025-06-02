// { dg-additional-options "-fsyntax-only" }

#include <coroutine>
#include <exception>

struct fire_and_forget {
};

template <typename... Args>
struct std::coroutine_traits<fire_and_forget, Args...>
{
    struct promise_type
    {
        fire_and_forget get_return_object() const noexcept
        {
            return{};
        }

        void return_void() const noexcept
        {
        }

        suspend_never initial_suspend() const noexcept
        {
            return{};
        }

        suspend_never final_suspend() const noexcept
        {
            return{};
        }

        void unhandled_exception() const noexcept
        {
            std::terminate();
        }
    };
};

struct foo
{
    fire_and_forget bar()
    {
        co_await std::suspend_always{ };
    }

private:
    // The line below triggered the error.
    using coroutine_handle = std::coroutine_handle<>;
};

int main()
{
    foo{}.bar();
}
