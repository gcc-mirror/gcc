//  { dg-do run }

#include "../coro.h"

struct pt_b
{
  std::suspend_never initial_suspend() const noexcept { return {}; }
  std::suspend_never final_suspend() const noexcept { return {}; }
  void unhandled_exception() const noexcept {}
  constexpr static void return_void () noexcept {}
};

int called_yv_op = 0;
int v;

struct pt_c : pt_b
{
  struct yv
    {
      auto operator ()(int x){
        PRINTF("call to operator yield val with %d\n", x);
        called_yv_op++;
        v = x;
        return std::suspend_never{};
    }
  };
  using handle_t = std::coroutine_handle<pt_c>;
  auto get_return_object() noexcept { return handle_t::from_promise(*this); }
  yv yield_value;
};

int called_lambda = 0;
struct pt_d : pt_b
{
  using handle_t = std::coroutine_handle<pt_d>;
  auto get_return_object() noexcept { return handle_t::from_promise(*this); }
  static constexpr auto yield_value = [] (int x) -> std::suspend_never
     {
	PRINTF("call to lambda yield val %d\n", x);
	called_lambda++;
	v = x;
	return {};
    };
};

template <> struct std::coroutine_traits<pt_c::handle_t>
    { using promise_type = pt_c; };

static pt_c::handle_t foo ()
{
    co_yield 5;
}

template <> struct std::coroutine_traits<pt_d::handle_t>
    { using promise_type = pt_d; };

static pt_d::handle_t bar ()
{
    co_yield 3;
}

int main ()
{
  /* These 'coroutines' run to completion imediately, like a regular fn.  */
  foo ();
  if (v != 5)
     {
      PRINT ("foo failed to yield v");
      abort ();
    }

  bar ();
  if (v != 3)
     {
      PRINT ("bar failed to yield v");
      abort ();
    }

  if (called_yv_op != 1 || called_lambda != 1)
    {
      PRINT ("Failed to call one of the yield_value cases");
      abort ();
    }
}
