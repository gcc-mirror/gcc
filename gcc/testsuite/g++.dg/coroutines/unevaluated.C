// { dg-additional-options "-fsyntax-only" }
#include <typeinfo>
#include <coroutine>

struct Task {
    struct promise_type {
        promise_type() = default;
        Task get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        void unhandled_exception() {}
        void return_void () {}
        std::suspend_never yield_value (int) { return {}; }
    };
};

// We do not permit co_await, co_yield outside a function, and so uses in
// noexcept or requirements are covered by that.
Task foo()  {
    /* This one will currently fail - see PR68604.  */
    const std::type_info& ti1 = typeid (co_await std::suspend_never{}); // { dg-error {'co_await' cannot be used in an unevaluated context} "" { xfail *-*-* } }
    std::size_t x = sizeof (co_yield (19)); // { dg-error {'co_yield' cannot be used in an unevaluated context} }
    decltype (co_await std::suspend_never{}) A; // { dg-error {'co_await' cannot be used in an unevaluated context} }
    co_return;
}
