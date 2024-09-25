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
        std::suspend_never yield_value(int x);
    };
};

// Similar to the above, but with a template yield_value and return_value.
struct D {
    struct promise_type {
        std::suspend_always initial_suspend();
        std::suspend_always final_suspend() noexcept;
        void unhandled_exception();
        D get_return_object();
        void return_value(auto);
        std::suspend_never yield_value(auto);
    };
};

template<typename>
C f() {
  co_return;
}

template<typename>
C g()
{
  co_yield 123;
}

template<typename>
D f1() {
  co_return 123;
}

template<typename>
D g1()
{
  co_yield 123;
}

void
g() {
  f<int>();
  f<bool>();
  g<int>();
  g<bool>();

  f1<int>();
  f1<bool>();
  g1<int>();
  g1<bool>();
}
