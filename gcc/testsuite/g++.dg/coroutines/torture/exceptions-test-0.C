//  { dg-do run }

// Test exceptions.

#include "../coro.h"
#include <exception>

int gX = 0;

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

  struct suspend_never_prt {
    bool await_ready() const noexcept { return true; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-never-susp"); }
    void await_resume() const noexcept { PRINT ("susp-never-resume");}
  };

  /* NOTE: this has a DTOR to test that pathway.  */
  struct  suspend_always_prt {
    bool await_ready() const noexcept { return false; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp"); }
    void await_resume() const noexcept { PRINT ("susp-always-resume"); }
    ~suspend_always_prt() { PRINT ("susp-always-DTOR"); }
  };

  struct promise_type {
  int value;
  promise_type() {  PRINT ("Created Promise"); }
  ~promise_type() { PRINT ("Destroyed Promise"); }

  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }
  auto initial_suspend () {
    PRINT ("get initial_suspend (always)");
    return suspend_always_prt{};
  }
  auto final_suspend () {
    PRINT ("get final_suspend (always)");
    return suspend_always_prt{};
  }
  void return_value (int v) {
    PRINTF ("return_value () %d\n",v);
    value = v;
  }
  auto yield_value (int v) {
    PRINTF ("yield_value () %d and suspend always\n",v);
    value = v;
    return suspend_always_prt{};
  }
  /* Some non-matching overloads.  */
  auto yield_value (suspend_always_prt s, int x) {
    return s;
  }
  auto yield_value (void) {
    return 42;//suspend_always_prt{};
  }
  int get_value (void) { return value; }

  void unhandled_exception() {
    PRINT ("unhandled_exception: caught one!");
    gX = -1;
    // returning from here should end up in final_suspend.
    }
  };
};

// So we want to check that the internal behaviour of try/catch is 
// working OK - and that if we have an unhandled exception it is caught
// by the wrapper that we add to the rewritten func.

struct coro1 throw_and_catch () noexcept
{
  int caught = 0;

  try {
    PRINT ("f: about to yield 42");
    co_yield 42;
 
    throw (20);

    PRINT ("f: about to yield 6174");
    co_return 6174;

  } catch (int x) {
    PRINTF ("f: caught %d\n", x);
    caught = x;
  }

  PRINTF ("f: about to yield what we caught %d\n", caught);
  co_yield caught;

  throw ("bah");

  PRINT ("f: about to return 22");
  co_return 22;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = throw_and_catch ();
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: got coro, resuming..");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  PRINT ("main: apparently got the expected 42");
  if (x.handle.done())
    abort();
  PRINT ("main: resuming...");
  x.handle.resume();

  y = x.handle.promise().get_value();
  if ( y != 20 )
    abort ();
  PRINT ("main: apparently got 20, which we expected");
  if (x.handle.done())
    abort();

  PRINT ("main: resuming...");
  x.handle.resume();
  // This should cause the throw of "bah" which is unhandled.
  // We should catch the unhandled exception and then fall through
  // to the final suspend point... thus be "done".
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  // When we caught the unhandled exception we flagged it instead of
  // std::terminate-ing.
  if (gX != -1)
    {
      PRINT ("main: apparently failed to catch");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
