//  { dg-do run }

#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std {
    using namespace std::experimental;
}
#endif
#include <cstdlib>

template <typename T>
struct generator{
    struct promise_type;
    using coro_handle = std::coroutine_handle<promise_type>;

    struct promise_type{
        std::suspend_always yield_value (T value){
            value_ = value;
            return {};
        }
        std::suspend_always initial_suspend (){
            return {};
        }
        std::suspend_always final_suspend () noexcept {
            return {};
        }

        std::suspend_never return_void()
        {
            return {};
        }
        generator get_return_object () {
            return {coro_handle::from_promise(*this)};
        }
        void unhandled_exception () {
            return;
        }
        T value_;
    };
    coro_handle handle;
    generator(coro_handle h)
        :handle(h)
    {}
    ~generator(){
        if(handle)
            handle.destroy();
    }

    bool resume(){
        if(not handle.done())
            handle.resume();
        return not handle.done();
    };

    T get () {
        return handle.promise().value_;
    }
};
namespace A
{
}

generator<int>
parse()
{
    namespace B = A;
    co_yield 1;
}

int main()
{
    auto gen = parse();
    gen.handle.resume (); /* init suspend. */
    if (gen.get() != 1)
      abort ();
  return 0;
}
