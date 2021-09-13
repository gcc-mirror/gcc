// { dg-do compile  { target c++17 } }

#include "coro.h"

// Test that we get matching types to traits and promise param
// preview.

// A separate issue from allowing non-class return types.
struct Fake {} ;

template<typename R, typename CallOp, typename ...T>
struct std::coroutine_traits<R, CallOp, T...> {
    struct promise_type {
        promise_type (CallOp op, T ...args) {}
        Fake get_return_object() { return {}; }
        std::suspend_always initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
    };
};


struct Foo
{
  Fake operator() (int a) {
    co_return;
  }
};
