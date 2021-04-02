//  { dg-additional-options "-Wall" }

#include <coroutine>

template <typename _Tp> struct promise;
template <typename _Tp> struct task {
	using promise_type = promise<_Tp>;
	bool await_ready() noexcept { return false; }
	void await_suspend(std::coroutine_handle<> awaiter) noexcept { m_a = awaiter; }
	auto await_resume() { return _Tp(); }
	std::coroutine_handle<> m_a;
};

template <typename _Tp> struct promise {
	auto get_return_object() noexcept { return task<_Tp>(); }
	auto initial_suspend() noexcept { return std::suspend_always(); }
	auto final_suspend() noexcept { return std::suspend_always(); }
	void return_value(_Tp value) noexcept { m_v = value; }
	void unhandled_exception() noexcept {}
	_Tp m_v;
};

task<int> test_coro(void) {
	int r = 0;
#if 1
	// this code causes the unexpected warning below
	r += co_await task<int>();
#else
	// this code causes no warning
	auto b = co_await task<int>();
	r += b;
#endif
	co_return r;
	// test1.cpp: In function ‘task<int> test_coro(int)’:
	// test1.cpp:36:1: warning: statement has no effect [-Wunused-value]
	//   36 | }
	//      | ^
}

int main(void) {
	return 0;
}
