// https://gcc.gnu.org/PR112341
#include <coroutine>
struct A { int j; };
struct B {
    bool await_ready();
    bool await_suspend(std::coroutine_handle<>);
    A await_resume();
};
struct C {
    struct promise_type {
        std::suspend_always initial_suspend();
        std::suspend_always final_suspend() noexcept;
        B yield_value(auto);
        void unhandled_exception();
        C get_return_object();
        void return_value(A);
    };
};
template<typename>
C f() {
  // Make sure we verify types we can still.
  (co_await B()).i; // { dg-error "'struct A' has no member named 'i'" }
  (co_yield 123).i; // { dg-error "'struct A' has no member named 'i'" }
  co_return 123; // { dg-error "cannot convert 'int' to 'A'" }
}
