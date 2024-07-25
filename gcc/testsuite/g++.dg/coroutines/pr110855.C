// { dg-do run }
// { dg-output {^} }
// { dg-output {ReturnObject bar\(int, char, bool\)(\n|\r\n|\r)} }
// { dg-output {ReturnObject bar\(int, char, bool\)(\n|\r\n|\r)} }
// { dg-output {ReturnObject bar\(int, char, bool\)(\n|\r\n|\r)} }
// { dg-output {ReturnObject bar\(int, char, bool\)(\n|\r\n|\r)} }
// { dg-output {ReturnObject bar\(int, char, bool\)(\n|\r\n|\r)} }
// { dg-output {$} }
// https://gcc.gnu.org/PR110855
#include <coroutine>
#include <source_location>

struct ReturnObject {
  struct promise_type {
    auto
    initial_suspend(const std::source_location location =
                    std::source_location::current()) {
      __builtin_puts (location.function_name ());
      return std::suspend_never{};
    }
    auto
    final_suspend(const std::source_location location =
                  std::source_location::current()) noexcept {
      __builtin_puts (location.function_name ());
      return std::suspend_never{};
    }
    auto
    get_return_object(const std::source_location location =
                      std::source_location::current()) {
      __builtin_puts (location.function_name ());
      return ReturnObject{std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    auto
    unhandled_exception() { }
    auto return_void(const std::source_location location =
                     std::source_location::current()) {
      __builtin_puts (location.function_name ());
    }
  };
  std::coroutine_handle<> handle;
};

struct awaitable : std::suspend_never
{
  void await_resume(const std::source_location location =
                     std::source_location::current())
  {
      __builtin_puts (location.function_name ());
  }
};

ReturnObject
bar(int, char, bool) {
  co_await awaitable{};
  co_return;
}

int
main() {
  bar(1, 'a', false);
}
