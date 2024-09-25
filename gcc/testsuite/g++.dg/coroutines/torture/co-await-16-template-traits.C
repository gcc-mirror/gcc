//  { dg-do compile }
//  { dg-skip-if "requires hosted libstdc++ for chrono" { ! hostedlib } }
//  Test we create co_await_expr with dependent type rather than type of awaitable class

#include "../coro.h"
#include "../coro1-ret-int-yield-int.h"
#include <chrono>

struct TestAwaiter {
    int recent_test;
    TestAwaiter(int test) : recent_test{test} {}
    bool await_ready() { return true; }
    void await_suspend(coro::coroutine_handle<>) {}
    int await_resume() { return recent_test;}
    void return_value(int x) { recent_test = x;}
};

template <typename Rep, typename Period>
coro1 test_temparg (std::chrono::duration<Rep, Period> dur)
{
       auto sum = co_await TestAwaiter(1);
       if (!sum)
	 dur.count();
       co_return 0;
}
