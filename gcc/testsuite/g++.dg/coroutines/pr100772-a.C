//  { dg-additional-options "-fsyntax-only " }
#if !__has_include(<coroutine>) \
  && __has_include(<experimental/coroutine>) // for __clang__
#include <experimental/coroutine>
namespace std {
  using namespace std::experimental;
}
#else
#include <coroutine>
#endif

struct Task
{
    struct promise_type
    {        
		void return_void() const noexcept {}

		void* operator new(std::size_t, auto &&...args) noexcept
		{
            static_assert(sizeof...(args) > 0);
            static_assert(sizeof...(args) == 2);

			return nullptr;
		}

		void operator delete(void *, std::size_t) noexcept
		{
		}

        static Task get_return_object_on_allocation_failure() noexcept
        {
            return {};
        }

        Task get_return_object() noexcept
        {
            return Task{ *this };
        }

        std::suspend_always initial_suspend() noexcept
        {
            return {};
        }

        std::suspend_always final_suspend() noexcept
        {
            return {};
        }

        void unhandled_exception() noexcept {}
    };

    using promise_handle = std::coroutine_handle<promise_type>;

    Task() = default;
    Task(promise_type & promise) noexcept
        : m_handle{ promise_handle::from_promise(promise) }
    {}

    ~Task()
    {
        if (m_handle.address()) { m_handle.destroy(); }
    }
    
    promise_handle m_handle{};
};


Task Foo(auto && ... args) noexcept
{
    co_return;
}

int main()
{
    int v;
    Foo(v, 2134);
}
