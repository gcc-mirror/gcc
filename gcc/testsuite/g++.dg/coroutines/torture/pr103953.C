// { dg-do run }
// https://gcc.gnu.org/PR103953
#include <coroutine>
#include <utility>

static int ctor_dtor_count = 0;

struct task {
    struct promise_type;

    using handle_type = std::coroutine_handle<promise_type>;

    task(handle_type h) : handle(h) {
        ctor_dtor_count++;
    }
    task(const task & t) : handle(t.handle) {
        ctor_dtor_count++;
    }
    task(task && t) : handle(std::move(t.handle)) {
        ctor_dtor_count++;
    }
    ~task() {
	if (--ctor_dtor_count < 0)
	    __builtin_abort ();
    }

    struct promise_type {
        auto get_return_object() {
            return task{handle_type::from_promise(*this)};
        }

        auto initial_suspend() {
            return std::suspend_always {};
        }

        auto unhandled_exception() {}

        auto final_suspend() noexcept {
            return std::suspend_always{};
        }

        void return_void() {}
    };

   handle_type handle;

   void await_resume() {
       handle.resume();
   }

   auto await_suspend(handle_type) {
       return handle;
   }

   auto await_ready() {
       return false;
   }
};

int main() {
    {
	task coroutine_A = []() ->task {
	    co_return;
	}();

	task coroutine_B = [&coroutine_A]() ->task {
	    co_await coroutine_A;
	}();

	coroutine_B.handle.resume();
    }

    if (ctor_dtor_count != 0)
	__builtin_abort ();
}
