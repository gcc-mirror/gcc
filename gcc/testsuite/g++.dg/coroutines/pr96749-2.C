//  { dg-additional-options "-Wall -Wno-maybe-uninitialized" }

#include <coroutine>

#if 1
// with a struct, GCC emits "statement has no effect"
struct S {};
#else
// no warning with built-in types
using S = int;
#endif

S Func1(int);

struct C {
	auto operator co_await() {
		struct Awaitable final {
			bool await_ready() const { return true; }
			std::coroutine_handle<> await_suspend(std::coroutine_handle<>) { return {}; }
			int await_resume() { return 42; }
		};
		return Awaitable{};
	}
};

struct Task {
	struct promise_type {
		auto initial_suspend() { return std::suspend_always{}; }
		auto final_suspend() noexcept { return std::suspend_always{}; }
		void get_return_object() {}
		void unhandled_exception() {}
	};
};

Task Func2() {
	Func1(co_await C());
}
