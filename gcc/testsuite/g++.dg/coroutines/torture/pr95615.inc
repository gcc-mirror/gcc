#if __has_include(<coroutine>)
#include <coroutine>
#else
#include <experimental/coroutine>
namespace std {
    using namespace std::experimental;
}
#endif
#include <cassert>
#include <cstdio>
#include <cstdlib>

bool frame_live = false;
bool promise_live = false;
bool gro_live = false;

struct X {};

int Y_live = 0;

struct Y
{
  Y () { std::puts("Y ()"); Y_live++; } 
  Y (const Y&) { std::puts("Y (const Y&)"); Y_live++; } 
  ~Y () { std::puts("~Y ()"); Y_live--; }
};

struct task {
    struct promise_type {
        void* operator new(size_t sz) {
            std::puts("operator new()");
            frame_live = true;
            return ::operator new(sz);
        }

        void operator delete(void* p, size_t sz) {
            std::puts("operator delete");
            frame_live = false;
            return ::operator delete(p, sz);
        }

        promise_type() {
            std::puts("promise_type()");
#if PROMISE_CTOR_THROWS
             throw X{};
#endif
            promise_live = true;
        }

        ~promise_type() {
            std::puts("~promise_type()");
            promise_live = false;
        }

        struct awaiter {
            bool await_ready() {
#if INITIAL_AWAIT_READY_THROWS
               throw X{};
#endif
               return false;
            }
            void await_suspend(std::coroutine_handle<>) {
#if INITIAL_AWAIT_SUSPEND_THROWS
                throw X{};
#endif
            }
            void await_resume() {
#if INITIAL_AWAIT_RESUME_THROWS
// this would be caught by unhandled_exception () which is tested
// elsewhere.
                throw X{};
#endif
            }
        };

        awaiter initial_suspend() {
#if INITIAL_SUSPEND_THROWS
            throw X{};
#endif
            return {};
        }

        task get_return_object() {
            std::puts("get_return_object()");
#if GET_RETURN_OBJECT_THROWS
            throw X{};
#endif
	    bool gro_live = true;
            return task{};
        }

        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() noexcept {}
        void unhandled_exception() noexcept {
            std::puts("unhandled_exception()");
        }
    };

    task() { std::puts("task()"); }
    ~task() { std::puts("~task()"); }
    task(task&&) { std::puts("task(task&&)"); }
};

task f(Y Val) {
    co_return;
}

int main() {
    bool failed = false;
    Y Val;
    try {
        f(Val);
    } catch (X) {
        std::puts("caught X");
        if (gro_live)
          std::puts("gro live"), failed = true;
        if (promise_live)
          std::puts("promise live"), failed = true;
        if (frame_live)
          std::puts("frame live"), failed = true;
    }

  if (Y_live != 1)
    std::printf("Y live %d\n", Y_live), failed = true;

  if (failed)
    abort() ;
}
