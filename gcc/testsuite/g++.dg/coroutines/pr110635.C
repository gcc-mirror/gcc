// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#define CASE 0
#include <coroutine>
#include <iostream>

struct Coroutine {

    struct promise_type;

    using handler_type = std::coroutine_handle<promise_type>;

    struct initial_suspend_awaiter {

        bool await_ready() noexcept {
            std::cout << "await_ready" << std::endl;
            return false;
        }
        
        void await_suspend(handler_type h) noexcept {
            std::cout << "await_suspend" << std::endl;
        }

#if CASE == 0
        struct await_resume_return_object {
            await_resume_return_object() noexcept {
                std::cout << "await_resume_return_object" << std::endl;
            }
            
            ~await_resume_return_object() noexcept {
                std::cout << "~await_resume_return_object" << std::endl;
            }
        };
#elif CASE == 1
        using await_resume_return_object = struct{};
#elif CASE == 2
        using await_resume_return_object = int;
#else
        using await_resume_return_object = void;
#endif
        await_resume_return_object await_resume() noexcept {
            std::cout << "await_resume" << std::endl;
#if CASE == 0 || CASE == 1 || CASE == 2
            return {};
#endif
        }

        initial_suspend_awaiter() noexcept {
            std::cout << "initial_suspend_awaiter" << std::endl;
        }

        ~initial_suspend_awaiter() noexcept {
            std::cout << "~initial_suspend_awaiter" << std::endl;
        }
    };

    struct promise_type {
        void return_void() noexcept {}
        void unhandled_exception() noexcept { std::terminate();}
        initial_suspend_awaiter initial_suspend() noexcept { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        Coroutine get_return_object() {
            return Coroutine{handler_type::from_promise(*this)};
        }
    };

    handler_type handler;
};

int main() {
    auto coro = []()->Coroutine { co_return; }();
    coro.handler.resume();
}
