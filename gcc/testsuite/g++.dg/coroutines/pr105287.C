// { dg-additional-options "-fanalyzer" }
// { dg-excess-errors "lots of analyzer output, but no ICE" }
namespace std {
template <typename _Result> struct coroutine_traits : _Result {};
template <typename = void> struct coroutine_handle {
  operator coroutine_handle<>();
};
}
struct coro1 {
  using handle_type = std::coroutine_handle<>;
  coro1(handle_type);
  struct suspend_always_prt {
    bool await_ready() noexcept;
    void await_suspend(handle_type) noexcept;
    void await_resume() noexcept;
  };
  struct promise_type {
    std::coroutine_handle<> ch_;
    auto get_return_object() { return ch_; }
    auto initial_suspend() { return suspend_always_prt{}; }
    auto final_suspend() noexcept { return suspend_always_prt{}; }
    void unhandled_exception();
  };
};
struct BoolAwaiter {
  BoolAwaiter(bool);
  bool await_ready();
  void await_suspend(std::coroutine_handle<>);
  bool await_resume();
};
struct IntAwaiter {
  IntAwaiter(int);
  bool await_ready();
  void await_suspend(std::coroutine_handle<>);
  int await_resume();
};
coro1 my_coro() {
 int a = 1;
 if (a == 0) {
   int b = 5;
   
 }
 {
   int c = 10;
 }
 co_await BoolAwaiter(true) && co_await IntAwaiter(a); 
 
 }
