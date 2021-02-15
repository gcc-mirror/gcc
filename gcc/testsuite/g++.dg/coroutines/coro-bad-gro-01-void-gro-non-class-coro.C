// Test handling of the case where we have a void g-r-o and a non-void
// and non-class-type ramp return.

#include "coro.h"

int g_promise = -1;

template<typename R, typename HandleRef, typename ...T>
struct std::coroutine_traits<R, HandleRef, T...> {
    struct promise_type {
        promise_type (HandleRef h, T ...args)
        { h = std::coroutine_handle<promise_type>::from_promise (*this);
          PRINT ("Created Promise");
          g_promise = 1;
        }
	~promise_type () { PRINT ("Destroyed Promise"); g_promise = 0;}
        void get_return_object() {}

        auto initial_suspend() {
          return std::suspend_always{};
         }
        auto final_suspend() noexcept { return std::suspend_never{}; }

        void return_void() {}
        void unhandled_exception() {}
    };
};

int
my_coro (std::coroutine_handle<>& h)
{
  PRINT ("coro1: about to return");
  co_return;
} // { dg-error {cannot initialize a return object of type 'int' with an rvalue of type 'void'} }

int main ()
{
  std::coroutine_handle<> h;
  int t = my_coro (h);

  if (h.done())
    {
      PRINT ("main: apparently was already done...");
      abort ();
    }

  // initial suspend.
  h.resume ();

  // The coro should have self-destructed.
  if (g_promise)
    {
      PRINT ("main: apparently we did not complete...");
      abort ();
    }

  PRINT ("main: returning");
  return t;
}
