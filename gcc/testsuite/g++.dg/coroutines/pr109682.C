
#include <coroutine>
#include <new>

struct test
{
  test () {}
  test (int) {}

  struct promise_type {
    test get_return_object () { return {}; }
    //     vvv
    static int get_return_object_on_allocation_failure () { return {}; }
    std::suspend_never initial_suspend () noexcept { return {}; }
    std::suspend_never final_suspend () noexcept { return {}; }
    void return_void () {}
    void unhandled_exception () {}
  };
};

test
f () { co_return; }

int
main ()
{
  f ();
}
