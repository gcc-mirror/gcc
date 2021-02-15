//  { dg-additional-options  "-Wall -O -fconcepts-ts" }

// This should complete without any diagnostic.

#include <coroutine>
#include <exception>

template <typename T>
class lazy {
    T _v = 0;
public:
    lazy() {}
    bool await_ready() {return true;}
    void await_suspend(auto x) noexcept {}
    T await_resume() { return _v; }
};

namespace std {

template <typename T, typename... Args>
struct coroutine_traits<lazy<T>, Args...> {
    struct promise_type {
        suspend_always initial_suspend() const { return {}; }
        suspend_always final_suspend() const noexcept { return {}; }
        void return_value(T val) {}
        lazy<T> get_return_object() {
            return lazy<T>();
        }
        void unhandled_exception() {
            std::terminate();
        }
    };
};
}

struct xxx {
    static lazy<int> func() {
        co_return 1;
    }
};

#if 0
lazy<int> foo() {
    co_return co_await xxx::func();
}
#endif
