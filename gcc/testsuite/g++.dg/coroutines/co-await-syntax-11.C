//  { dg-additional-options "-std=c++17 -w" }

#include <utility>
#include <type_traits>
#include <tuple>
#include <functional>
#include <coroutine>

struct any {
  template <typename T> any(T &&) noexcept;
};

template <typename T>
auto get_awaiter_impl(T &&value, int) noexcept
    -> decltype(static_cast<T &&>(value).operator co_await()) {
  return static_cast<T &&>(value).operator co_await();
}
template <typename T, int = 0>
T &&get_awaiter_impl(T &&value, any) noexcept;
template <typename T>
auto get_awaiter(T &&value) noexcept
    -> decltype(get_awaiter_impl(static_cast<T &&>(value), 123)) {
  return get_awaiter_impl(static_cast<T &&>(value), 123);
}

template <typename T, typename = void> struct awaitable_traits {
  using awaiter_t = decltype(get_awaiter(std::declval<T>()));
  using await_result_t = decltype(std::declval<awaiter_t>().await_resume());
};

template <typename TASK_CONTAINER> class when_all_ready_awaitable;
template <typename... TASKS>
class when_all_ready_awaitable<std::tuple<TASKS...>> {
public:
  explicit when_all_ready_awaitable(std::tuple<TASKS...> &&tasks) noexcept
    : m_tasks(std::move(tasks)) {}
  auto operator co_await() &&noexcept {
    struct awaiter {
      awaiter(when_all_ready_awaitable &awaitable) noexcept
        : m_awaitable(awaitable) {}
      bool await_ready() const noexcept { return false; }
      bool await_suspend() noexcept { return false; }
      std::tuple<TASKS...> &&await_resume() noexcept {
        return std::move(m_awaitable.m_tasks);
      }
      when_all_ready_awaitable& m_awaitable;
    };
    return awaiter{*this};
  }
  std::tuple<TASKS...> m_tasks;
};

inline void *operator new(std::size_t, void *__p) noexcept;

template <typename RESULT>
class when_all_task_promise final{
public:
  using coroutine_handle_t = std::coroutine_handle<when_all_task_promise>;
  RESULT &&result() &&;
};
template <typename RESULT> class when_all_task final {
public:
  using promise_type = when_all_task_promise<RESULT>;
  using coroutine_handle_t = typename promise_type::coroutine_handle_t;
  decltype(auto) result() &;
  decltype(auto) result() && {
    return std::move(m_coroutine.promise()).result();
  }
  decltype(auto) non_void_result() && {
    if constexpr (std::is_void_v<decltype(0)>)
      ;
    else
      return std::move(*this).result();
  }
  coroutine_handle_t m_coroutine;
};
class task;
template <typename AWAITABLE,
          typename RESULT = 
              typename awaitable_traits<AWAITABLE &&>::await_result_t,
          std::enable_if_t<!std::is_void_v<RESULT>, int> = 0>
when_all_task<RESULT> make_when_all_task(AWAITABLE awaitable);

template <typename... AWAITABLES>
inline auto when_all_ready(AWAITABLES &&... awaitables) {
  return when_all_ready_awaitable<
      std::tuple<when_all_task<typename awaitable_traits<
          std::remove_reference_t<AWAITABLES>>::await_result_t>...>>(
      std::make_tuple(
          make_when_all_task(std::forward<AWAITABLES>(awaitables))...));
}

template <typename FUNC, typename AWAITABLE> class fmap_awaiter {
  using awaiter_t = typename awaitable_traits<AWAITABLE &&>::awaiter_t;

public:
  fmap_awaiter(FUNC &&func, AWAITABLE &&awaitable) noexcept
      : m_func(static_cast<FUNC &&>(func)),
        m_awaiter(get_awaiter(static_cast<AWAITABLE &&>(awaitable))) {}
  decltype(auto) await_ready() noexcept {
    return static_cast<awaiter_t &&>(m_awaiter).await_ready();
  }
  template <typename PROMISE>
  decltype(auto) await_suspend(std::coroutine_handle<PROMISE> coro) noexcept {}
  template <typename AWAIT_RESULT =
                decltype(std::declval<awaiter_t>().await_resume()),
            std::enable_if_t<!std::is_void_v<AWAIT_RESULT>, int> = 0>
  decltype(auto) await_resume() noexcept {
    return std::invoke(static_cast<FUNC &&>(m_func),
                       static_cast<awaiter_t &&>(m_awaiter).await_resume());
  }

private:
  FUNC &&m_func;
  awaiter_t m_awaiter;
};
template <typename FUNC, typename AWAITABLE> class fmap_awaitable {
public:
  template <
      typename FUNC_ARG, typename AWAITABLE_ARG,
      std::enable_if_t<std::is_constructible_v<FUNC, FUNC_ARG &&> &&
                           std::is_constructible_v<AWAITABLE, AWAITABLE_ARG &&>,
                       int> = 0>
  explicit fmap_awaitable(FUNC_ARG &&func, AWAITABLE_ARG &&awaitable) noexcept
      : m_func(static_cast<FUNC_ARG &&>(func)),
        m_awaitable(static_cast<AWAITABLE_ARG &&>(awaitable)) {}
  auto operator co_await() && {
    return fmap_awaiter(static_cast<FUNC &&>(m_func),
                        static_cast<AWAITABLE &&>(m_awaitable));
  }

private:
  FUNC m_func;
  AWAITABLE m_awaitable;
};

template <typename FUNC, typename AWAITABLE>
auto fmap(FUNC &&func, AWAITABLE &&awaitable) {
  return fmap_awaitable<std::remove_cv_t<std::remove_reference_t<FUNC>>,
                        std::remove_cv_t<std::remove_reference_t<AWAITABLE>>>(
      std::forward<FUNC>(func), std::forward<AWAITABLE>(awaitable));
}
template <typename... AWAITABLES>
auto when_all(AWAITABLES &&... awaitables) {
  return fmap(
      [](auto &&taskTuple) {
        decltype(auto) __trans_tmp_1 = std::apply(
            [](auto &&... tasks) {
              return std::make_tuple(
                  static_cast<decltype(tasks)>(tasks).non_void_result()...);
            },
            static_cast<decltype(taskTuple)>(taskTuple));
        return __trans_tmp_1;
      },
      when_all_ready(std::forward<AWAITABLES>(awaitables)...));
}
class async_mutex_scoped_lock_operation;
class async_mutex {
public:
  async_mutex() noexcept;
  async_mutex_scoped_lock_operation scoped_lock_async() noexcept;
};
class async_mutex_lock {
public:
  explicit async_mutex_lock();
  ~async_mutex_lock();

private:
  async_mutex *m_mutex;
};
class async_mutex_scoped_lock_operation {
public:
  async_mutex_lock await_resume() const noexcept;
};
class task {
public:
  class promise_type {
  public:
    auto initial_suspend() noexcept { return std::suspend_always{}; }
    auto final_suspend() noexcept { return std::suspend_always{}; }
    task get_return_object() noexcept { return task{}; }
    void unhandled_exception() noexcept {}
    void return_value(int value) noexcept { v = value; }
    int result(){ return v; }
    int v = 0;
  };
public:
  task() noexcept {}
  auto operator co_await() const &noexcept {
    struct awaitable {
      std::coroutine_handle<promise_type> m_coroutine;
      decltype(auto) await_resume() {
        return this->m_coroutine.promise().result();
      }
    };
    return awaitable{};
  }
};
void foo() {
  (void) []() -> task {
    auto makeTask = [](int x) -> task { co_return x; };
    async_mutex_scoped_lock_operation op;
    co_await when_all(std::move(op), makeTask(123));
  }();
}
