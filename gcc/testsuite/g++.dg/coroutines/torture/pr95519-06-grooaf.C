//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

struct pt_b
{
  constexpr std::suspend_never initial_suspend() noexcept { return {}; }
  constexpr std::suspend_never final_suspend() noexcept { return {}; }
  constexpr void return_void () noexcept {} 
  constexpr void unhandled_exception() noexcept {}
};

int called_grooaf = 0;

struct pt_c : pt_b
{
  using handle_t = std::coroutine_handle<pt_c>;
  auto get_return_object() noexcept { return handle_t::from_promise(*this); }

  static constexpr auto get_return_object_on_allocation_failure
      = []{ PRINT("call to lambda grooaf");
	    called_grooaf++; return std::coroutine_handle<pt_c>{};
	  };

  /* Provide an operator new, that always fails.  */
  void *operator new (std::size_t sz) noexcept {
    PRINT ("promise_type: used failing op new");
    return nullptr;
  }
};

template <> struct std::coroutine_traits<pt_c::handle_t>
    { using promise_type = pt_c; };

static pt_c::handle_t
foo ()
{
  co_return;
}

int main ()
{
  foo ();
  if (called_grooaf != 1)
    {
      PRINT ("Failed to call grooaf");
      abort ();
    }
}
