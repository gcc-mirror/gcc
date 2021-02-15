// Check that we obey the extra rules for implicitly movable co_return
// objects [class.copy.elision]/3.

#include "coro.h"

#include  <utility>

template <typename T>
struct coro1 {
  struct promise_type;
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  handle_type handle;
  coro1 () : handle(0) {}
  coro1 (handle_type _handle)
    : handle(_handle) { }
  coro1 (const coro1 &) = delete; // no copying
  coro1 (coro1 &&s) : handle(s.handle) { s.handle = nullptr;  }
  coro1 &operator = (coro1 &&s) {
    handle = s.handle;
    s.handle = nullptr;
    return *this;
  }
  ~coro1() {
    if ( handle )
      handle.destroy();
  }

  struct promise_type {
  T value;
  promise_type() {}
  ~promise_type() {}

  auto get_return_object () { return handle_type::from_promise (*this);}
  coro::suspend_always initial_suspend () const { return {}; }
  coro::suspend_always final_suspend () const noexcept {  return {}; }

  void return_value(T&& v) noexcept { value = std::move(v); }
  
  T get_value (void) { return value; }
  void unhandled_exception() { }
  };
};

struct MoveOnlyType 
{
  int value_;

  explicit MoveOnlyType() noexcept : value_(0) {}
  explicit MoveOnlyType(int value) noexcept : value_(value) {}

  MoveOnlyType(MoveOnlyType&& other) noexcept
      : value_(std::exchange(other.value_, -1)) {}

  MoveOnlyType& operator=(MoveOnlyType&& other) noexcept {
    value_ = std::exchange(other.value_, -1);
    return *this;
  }

  ~MoveOnlyType() { value_ = -2; }
};

coro1<MoveOnlyType> 
my_coro ()
{
  MoveOnlyType x{10};
  co_return x;
}
