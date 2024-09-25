//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

struct pt_b
{
  constexpr std::suspend_never initial_suspend() const noexcept { return {}; }
  constexpr std::suspend_never final_suspend() const noexcept { return {}; }
  constexpr void return_void () {};
};

int called_ueh_op = 0;
struct ueh
{
  void operator ()() noexcept {
    PRINT("call to operator UEH");
    called_ueh_op++;
  }
};

struct pt_c : pt_b
{
  using handle_t = std::coroutine_handle<pt_c>;
  auto get_return_object() noexcept { return handle_t::from_promise(*this); }
  ueh unhandled_exception;
};

int lambda_ueh = 0;

struct pt_d : pt_b
{
  using handle_t = std::coroutine_handle<pt_d>;
  auto get_return_object() noexcept { return handle_t::from_promise(*this); }
  static constexpr auto unhandled_exception
    = [] () noexcept { PRINT("call to lambda UEH"); lambda_ueh++; };
};

template <>
struct std::coroutine_traits<pt_c::handle_t>
    { using promise_type = pt_c; };

static pt_c::handle_t
foo ()
{
  throw ("foo");
  co_return;
}

template <> 
struct std::coroutine_traits<pt_d::handle_t>
    { using promise_type = pt_d; };

static pt_d::handle_t
bar ()
{
  throw ("bar");
  co_return;
}

int main ()
{
  foo ();
  bar ();
  if (called_ueh_op != 1 || lambda_ueh != 1)
    {
      PRINT ("Failed to call one of the UEH cases");
      abort ();
    }
}
