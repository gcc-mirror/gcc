// PR120453 - reduced testcase amended to add a TaskBase move constructor
// and a LazyTask destructor, to more closely match the original code.
// { dg-additional-options "-w" }
namespace std {
template <typename> struct __coroutine_traits_impl;
template <typename _Result>
  requires requires { typename _Result; }
struct __coroutine_traits_impl<_Result>;
template <typename _Result, typename> struct coroutine_traits : _Result {};
template <typename = void> struct coroutine_handle {
  static coroutine_handle from_address(void *);
  operator coroutine_handle<>();
  void *address();
};
struct suspend_never {
  bool await_ready();
  void await_suspend(coroutine_handle<>);
  void await_resume();
};
} // namespace std

namespace QCoro {
namespace detail {
template <typename T>
concept has_await_methods = requires(T t) { t; };
} // namespace detail

template <typename T>
concept Awaitable = detail::has_await_methods<T>;
namespace detail {
struct TaskFinalSuspend {
  bool await_ready() noexcept;
  template <typename Promise>
  void await_suspend(std::coroutine_handle<Promise>) noexcept;
  void await_resume() noexcept;
};
struct TaskPromiseBase {
  std::suspend_never initial_suspend();
  auto final_suspend() noexcept { return TaskFinalSuspend{}; }
  template <Awaitable T> auto &&await_transform(T &&);
};
struct TaskPromise : TaskPromiseBase {
  void unhandled_exception();
};
template <typename> struct TaskAwaiterBase {
  bool await_ready();
  void await_suspend(std::coroutine_handle<>);
};
template <typename, template <typename> class, typename> struct TaskBase {
  TaskBase() = default;
  TaskBase(TaskBase &&) = default;
  void operator=(TaskBase &&);
  auto operator co_await() const;
  std::coroutine_handle<> mCoroutine;
};
} // namespace detail
template <typename> struct Task : detail::TaskBase<int, Task, int> {};
} // namespace QCoro

namespace QCoro::detail {
template <Awaitable T> auto &&TaskPromiseBase::await_transform(T &&awaitable) {
  return awaitable;
}

template <typename T, template <typename> class TaskImpl, typename PromiseType>
auto TaskBase<T, TaskImpl, PromiseType>::operator co_await() const {
  class TaskAwaiter : public TaskAwaiterBase<PromiseType> {
  public:
    TaskAwaiter(std::coroutine_handle<>);
    auto await_resume() {}
  };
  return TaskAwaiter{mCoroutine};
}

} // namespace QCoro::detail

namespace QCoro {
template <typename T> class LazyTask ;
namespace detail {
struct LazyTaskPromise : TaskPromise {
  LazyTask<int> get_return_object();
};
} // namespace detail

template <typename T> struct LazyTask : detail::TaskBase<int, LazyTask, int> {
  typedef detail::LazyTaskPromise promise_type;
  ~LazyTask();
};

auto coro = [] -> LazyTask<int>
{
  co_await [] -> Task<int> { return {}; }();
};

} // namespace QCoro
