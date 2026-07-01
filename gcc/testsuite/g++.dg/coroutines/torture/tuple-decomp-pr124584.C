#include <coroutine>
#include <tuple>

static int g_dtor_count = 0;

struct guard {
  bool live_ = true;
  ~guard() { if (live_) ++g_dtor_count; }
  guard() = default;
  guard(guard&& o) noexcept
    : live_(o.live_) {o.live_ = false;}
};

static std::coroutine_handle<> g_pending;

struct yield_aw {
  bool await_ready() const noexcept { return false; }
  void await_suspend(std::coroutine_handle<> h) noexcept
  { g_pending = h; }
  void await_resume() const noexcept {}
};

struct task {
  struct promise_type {
    task get_return_object() noexcept {
      return {std::coroutine_handle<
	promise_type>::from_promise(*this)};
    }
    std::suspend_never  initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend()   noexcept { return {}; }
    void return_void() noexcept {}
    void unhandled_exception() { __builtin_abort(); }
  };
  std::coroutine_handle<promise_type> h_;
  ~task() { if (h_) h_.destroy(); }
};

std::tuple<guard> make_guarded_tuple() { return {guard{}}; }

task test_fn() {
  auto [g] = make_guarded_tuple();   // not co_await
  co_await yield_aw{};                // binding crosses suspend
  (void)g;
}

int main() {
  auto t = test_fn();
  auto h = g_pending;
  g_pending = nullptr;
  h.resume();
  __builtin_printf("dtor_count = %d  %s\n",
		   g_dtor_count, g_dtor_count > 0 ? "OK" : "BUG");
  return g_dtor_count > 0 ? 0 : 1;
}
