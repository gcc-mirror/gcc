// { dg-do compile { target c++20 } }
#include <coroutine>

struct Task {
    struct promise_type {
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void unhandled_exception() { throw; }
        Task get_return_object() { return {}; }
        void return_void() {}

        template<class I>
        void* operator new(std::size_t sz, I);

        void operator delete(void* ptr, std::size_t);
    };
};

Task f(int) {
    co_return;
}

int main() {
    f(42);
}
