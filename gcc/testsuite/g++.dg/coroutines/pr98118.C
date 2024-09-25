namespace std {
inline namespace __n4861 {
template <typename _Result, typename> struct coroutine_traits : _Result {};
template <typename = void> struct coroutine_handle;
template <> struct coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
template <typename> struct coroutine_handle : coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
struct suspend_never {
  bool await_ready() noexcept;
  void await_suspend(coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};
} // namespace __n4861
} // namespace std

struct fire_and_forget {
  struct promise_type {
    fire_and_forget get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
};

struct bug {
  ~bug();
};

fire_and_forget f(bug) { co_return; }
