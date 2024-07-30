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
        void unhandled_exception();
        C get_return_object();
        void return_void();
        B yield_value(auto) { return {}; }
    };
};
C f1(auto) { (co_await B()).j; }
C f2(auto) { (co_yield 0).j; }
void g() { f1(0); f2(0); }
