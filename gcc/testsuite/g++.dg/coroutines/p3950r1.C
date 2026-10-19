// C++29 P3950R1 - return_value & return_void Are Not Mutually Exclusive

#include <coroutine>
#include <type_traits>

struct dummy_coroutine1 {};
template <typename T>
struct dummy_coroutine2 {};

namespace std {

template<>
class coroutine_traits<::dummy_coroutine1> {
public:
  struct promise_type {
    void return_value(int x) {}
    void return_value(double x) {}
    void return_void() {}
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    dummy_coroutine1 get_return_object() { return {}; }
    void unhandled_exception() {}
  };
};

template<typename T>
class coroutine_traits<::dummy_coroutine2<T>> {
public:
  struct promise_type {
    template<typename U = T>
    void return_value(U x) requires std::is_same_v<U, T> {}
    void return_void() requires std::is_same_v<void, T> {}
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    dummy_coroutine2<T> get_return_object() { return {}; }
    void unhandled_exception() {}
  };
};

}

int x;

dummy_coroutine1
foo ()
{
  if (x == 1)
    co_return 17;
  else if (x == 2)
    co_return 17.0;
  else if (x == 3)
    co_return;
}

dummy_coroutine2<void>
bar ()
{
  if (x == 4)
    co_return;
}

dummy_coroutine2<int>
baz ()
{
  if (x == 5)
    co_return 42;
}

dummy_coroutine2<double>
qux ()
{
  if (x == 6)
    co_return 42.0;
}
