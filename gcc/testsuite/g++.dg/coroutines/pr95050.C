#if __has_include (<coroutine>)
#include <coroutine>
using namespace std;
#elif defined (__clang__) && __has_include (<experimental/coroutine>)
#include <experimental/coroutine>
using namespace std::experimental;
#endif
#include <utility>

struct ret_type 
{
  ret_type () = default;
  ret_type (const ret_type&) = delete;
  //ret_type (ret_type&&) = default;
  ~ret_type() {}
};

struct task
{
  struct promise_type
  {
    auto get_return_object () -> task  { return {}; }
    auto initial_suspend () -> suspend_always { return {}; }
    auto final_suspend () -> suspend_always { return {}; }
    void return_void () {} 
    void unhandled_exception () { }
    void thing (ret_type x) {} 
  };
};

struct awaiter
{
  bool await_ready() const { return true; }
  void await_suspend (coroutine_handle<>) {}
  ret_type await_resume() { return {}; }
};

task
my_coro ()
{
  ret_type r2{co_await awaiter{}};
  //ret_type r3 (std::move(r2));
}

int main()
{
 auto x = my_coro ();
 return 0;
}
