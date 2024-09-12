//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

struct pt_b
{
  std::suspend_always initial_suspend() const noexcept { return {}; }
  std::suspend_always final_suspend() const noexcept { return {}; }
  constexpr void return_void () noexcept {};
  constexpr void unhandled_exception() const noexcept {}
};

int called_gro_op = 0;

template<typename R, typename HandleRef, typename ...T>
struct std::coroutine_traits<R, HandleRef, T...> {
  struct pt_c;
  using promise_type = pt_c;
  struct pt_c : pt_b {
    //using handle_t = std::coroutine_handle<pt_c>;
    pt_c (HandleRef h, T ...args)
    {  h = std::coroutine_handle<pt_c>::from_promise (*this);
       PRINT ("Created Promise");
       //g_promise = 1;
    }
    struct gro
      {
        auto operator ()() {
        PRINT("call to operator ");
        called_gro_op++;
        }
      };
    gro get_return_object;
  };
};

static void
foo (std::coroutine_handle<>& h)
{
  co_return;
}

int main ()
{
  std::coroutine_handle<> f;
  foo (f);
  if (f.done())
    {
      PRINT ("unexpected finished foo coro");
      abort ();
   }
  f.resume();
  if (!f.done())
    {
      PRINT ("expected foo to be finished");
      abort ();
   }

  if (called_gro_op != 1)
    {
      PRINT ("Failed to call gro op");
      abort ();
   }
}
