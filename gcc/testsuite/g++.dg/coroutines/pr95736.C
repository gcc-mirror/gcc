#include <iostream>
#include <exception>
#include <cassert>

#if __has_include("coroutine")
#include <coroutine>
namespace stdcoro = std;
#else
#include <experimental/coroutine>
namespace stdcoro = std::experimental;
#endif

struct footable : stdcoro::suspend_always {
	footable() noexcept = default;
	~footable() { assert(released); }
	footable(const footable&) = delete;

	using coro_handle = stdcoro::coroutine_handle<>;

	void await_suspend(coro_handle awaiter) noexcept {
		std::cout << "suspending to footable " << this << std::endl;
		assert(!handle);
		handle = awaiter;
	}
	void await_resume() noexcept {
		std::cout << "resuming from footable " << this << std::endl;
		assert(handle);
		handle = {};
	}

	void operator()() noexcept {
		std::cout << "operator() on " << this << std::endl;
		assert(handle);
		handle.resume();
		handle = {};
	}

	void release() noexcept { released = true; }
private:
	coro_handle handle;
	bool released = false;
};

struct footask {
	struct promise_type {
		using coro_handle = stdcoro::coroutine_handle<promise_type>;

		stdcoro::suspend_never initial_suspend() noexcept { return {}; }
		stdcoro::suspend_never final_suspend() noexcept { std::cout << "final suspend" << std::endl; return {}; }
		void unhandled_exception() {}
		void return_void() noexcept { std::cout << "coro returns" << std::endl; }

		footask get_return_object() { return footask{ coro_handle::from_promise(*this) }; }
	};

	footask(promise_type::coro_handle handle) : handle(handle) {}
	~footask() { assert(handle.done()); }

	promise_type::coro_handle handle;
};

struct bar {
	bar() = default;
	bar(const bar&) = delete;

	footable foo{};
	footask task = taskfun();

	footask taskfun() noexcept {
		std::cout << "coro begin" << std::endl;
		co_await foo;
		std::cout << "coro end" << std::endl;
	}
};

int main() {
	bar foobar;
	foobar.foo();
	assert(foobar.task.handle.done());
	std::cout << "releasing" << std::endl;
	foobar.foo.release();
	std::cout << "done" << std::endl;
	return 0;
}
