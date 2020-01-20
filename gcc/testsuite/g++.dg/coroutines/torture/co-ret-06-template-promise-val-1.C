//  { dg-do run }

// Test returning a T.
// We will use the promise to contain this to avoid having to include
// additional C++ headers.

#include "../coro.h"

struct suspend_never_prt {
  bool await_ready() const noexcept { return true; }
  void await_suspend(coro::coroutine_handle<>) const noexcept
    { PRINT ("susp-never-susp"); }
  void await_resume() const noexcept { PRINT ("susp-never-resume");}
};

/* NOTE: this has a DTOR to test that pathway.  */
struct  suspend_always_prt {
  bool await_ready() const noexcept { return false; }
  void await_suspend(coro::coroutine_handle<>) const noexcept
    { PRINT ("susp-always-susp"); }
  void await_resume() const noexcept { PRINT ("susp-always-resume"); }
  ~suspend_always_prt() { PRINT ("susp-always-DTOR"); }
};

template <typename T>
struct coro1 {
  struct promise_type;
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  handle_type handle;
  coro1 () : handle(0) {}
  coro1 (handle_type _handle)
    : handle(_handle) {
    PRINT("Created coro1 object from handle");
  }
  coro1 (const coro1 &) = delete; // no copying
  coro1 (coro1 &&s) : handle(s.handle) {
    s.handle = nullptr;
    PRINT("coro1 mv ctor ");
  }
  coro1 &operator = (coro1 &&s) {
    handle = s.handle;
    s.handle = nullptr;
    PRINT("coro1 op=  ");
    return *this;
  }
  ~coro1() {
    PRINT("Destroyed coro1");
    if ( handle )
      handle.destroy();
  }

  struct promise_type {
  T value;
  promise_type() {  PRINT ("Created Promise"); }
  ~promise_type() { PRINT ("Destroyed Promise"); }

  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }

  auto initial_suspend () const {
    PRINT ("get initial_suspend (always)");
    return suspend_always_prt{};
  }
  auto final_suspend () const {
    PRINT ("get final_suspend (always)");
    return suspend_always_prt{};
  }
  void return_value (T v) {
    PRINTF ("return_value () %d\n",v);
    value = v;
  }
  T get_value (void) { return value; }
  void unhandled_exception() { PRINT ("** unhandled exception"); }
  };
};

coro1<float>
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return (float) 42;
}

int main ()
{
  PRINT ("main: create coro1");
  coro1<float> x = f ();
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != (float)42 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
