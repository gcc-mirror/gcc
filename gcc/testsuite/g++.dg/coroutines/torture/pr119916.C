// PR c++/119916
// { dg-do run }
// { dg-additional-options "-std=c++23" }

namespace folly {
struct OptionalPromise;
struct Optional {
  typedef OptionalPromise promise_type;
  Optional() { hasValue = 0; }
  Optional(int) { hasValue = 1; }
  bool has_value() { return hasValue; }
  int emptyState;
  int value;
  bool hasValue;
};
} // namespace folly
namespace std {
template <typename _Result, typename> struct coroutine_traits : _Result {};
template <typename = void> struct coroutine_handle;
template <> struct coroutine_handle<> {
  void *address();
};
template <typename> struct coroutine_handle {
  static coroutine_handle from_address(void *) {
    coroutine_handle __self;
    return __self;
  }
  coroutine_handle<> __trans_tmp_1;
  operator coroutine_handle<>() { return __trans_tmp_1; }
};
struct suspend_never {
  bool await_ready() noexcept { return true; }
  void await_suspend(coroutine_handle<>) noexcept {}
  void await_resume() noexcept {}
};
} // namespace std
namespace folly {
struct OptionalPromiseReturn;
struct OptionalPromise {
  Optional *value_;
  OptionalPromiseReturn get_return_object();
  std::suspend_never initial_suspend() { return {}; }
  std::suspend_never final_suspend() noexcept { return {}; }
  template <typename U> void return_value(U u) { *value_ = u; }
  void unhandled_exception() {}
};
struct OptionalPromiseReturn {
  Optional storage_;
  Optional *&pointer_;
  OptionalPromiseReturn(OptionalPromise &p) : pointer_{p.value_} {
    pointer_ = &storage_;
  }
  operator Optional() { return storage_; }
};
OptionalPromiseReturn OptionalPromise::get_return_object() {
    return *this;
}
} // namespace folly
folly::Optional main_r0 = [] -> folly::Optional {
  auto z(0);
  co_return z;
}();
int main() {
  if (!main_r0.has_value())
    __builtin_abort();
}
