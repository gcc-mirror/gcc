// PR c++/121219
// { dg-do run }

#include <coroutine>
#ifdef OUTPUT
#include <iostream>
#endif
#include <stdexcept>

struct Task {
    struct promise_type;
    using handle_type = std::coroutine_handle<promise_type>;

    struct promise_type {
        Task* task_;
        int result_;

        static void* operator new(std::size_t size) noexcept {
            void* p = ::operator new(size, std::nothrow);
#ifdef OUTPUT
            std::cerr << "operator new (no arg) " << size << " -> " << p << std::endl;  
#endif
            return p;
        }
        static void operator delete(void* ptr) noexcept {
            return ::operator delete(ptr, std::nothrow);
        }
#if 1 // change to 0 to fix crash
        static Task get_return_object_on_allocation_failure() noexcept {
#ifdef OUTPUT
            std::cerr << "get_return_object_on_allocation_failure" << std::endl;
#endif
            return Task(nullptr);
        }
#endif

        auto get_return_object() { 
#ifdef OUTPUT
            std::cerr << "get_return_object" << std::endl;
#endif
            return Task{handle_type::from_promise(*this)}; 
        }

        auto initial_suspend() { 
#ifdef OUTPUT
            std::cerr << "initial_suspend" << std::endl;
#endif
            return std::suspend_always{}; 
        }

        auto final_suspend() noexcept { 
#ifdef OUTPUT
            std::cerr << "final_suspend" << std::endl;
#endif
            return std::suspend_never{};  // Coroutine auto-destructs
        }

        ~promise_type() {
            if (task_) {
#ifdef OUTPUT
                std::cerr << "promise_type destructor: Clearing Task handle" << std::endl;
#endif
                task_->h_ = nullptr;
            }
        }

        void unhandled_exception() { 
#ifdef OUTPUT
            std::cerr << "unhandled_exception" << std::endl;
#endif
            std::terminate(); 
        }

        void return_value(int value) { 
#ifdef OUTPUT
            std::cerr << "return_value: " << value << std::endl;
#endif
            result_ = value;
            if (task_) {
                task_->result_ = value;
                task_->completed_ = true;
            }
        }
    };

    handle_type h_;
    int result_;
    bool completed_ = false;

    Task(handle_type h) : h_(h) {
#ifdef OUTPUT
        std::cerr << "Task constructor" << std::endl;
#endif
        if (h_) {
            h_.promise().task_ = this;  // Link promise to Task
        }
    }

    ~Task() { 
#ifdef OUTPUT
        std::cerr << "~Task destructor" << std::endl;
#endif
        // Only destroy handle if still valid (coroutine not completed)
        if (h_) {
#ifdef OUTPUT
            std::cerr << "Destroying coroutine handle" << std::endl;
#endif
            h_.destroy(); 
        }
    }

    bool done() const { 
        return completed_ || !h_ || h_.done(); 
    }

    void resume() { 
#ifdef OUTPUT
        std::cerr << "Resuming task" << std::endl;
#endif
        if (h_) h_.resume(); 
    }

    int result() const {
        if (!done()) throw std::runtime_error("Result not available");
        return result_;
    }
};

Task my_coroutine() {
#ifdef OUTPUT
    std::cerr << "Inside my_coroutine" << std::endl;
#endif
   co_return 42;
}

int main() {
    auto task = my_coroutine();
    while (!task.done()) {
#ifdef OUTPUT
        std::cerr << "Resuming task in main" << std::endl;
#endif
        task.resume();
    }
#ifdef OUTPUT
    std::cerr << "Task completed in main, printing result" << std::endl;
#endif
    if (task.result() != 42)
      __builtin_abort ();
}
