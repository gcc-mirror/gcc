//  { dg-do run }

// Test exceptions in the initial await expression, per n4849.

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

  struct  suspend_always_prt {
    bool await_ready() const noexcept { return false; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp"); }
    void await_resume() const noexcept { PRINT ("susp-always-resume"); }
    ~suspend_always_prt() { PRINT ("susp-always-DTOR"); }
  };

  /* Constructing this with:
      * a value of '1' will cause the initial suspend await_suspend()
      call to throw.
      * a value of '2' will cause the await resume to throw.  */
  struct  suspend_always_susp_throws_prt {
    int thrower;
    suspend_always_susp_throws_prt (int _t) : thrower(_t) {}
    bool await_ready() const noexcept { return false; }

    void await_suspend(handle_type) const
      { PRINT ("suspend_always_susp_throws_prt:await_suspend");
        if (thrower == 1)
          throw (42);
      }
    void await_resume() const
      { PRINT ("suspend_always_susp_throws_prt:await_resume");
        if (thrower == 2)
          throw (6174);
      }
    ~suspend_always_susp_throws_prt() { PRINT ("suspend_always_susp_throws_prt-DTOR"); }
  };

  struct promise_type {
  int throw_control = 0;
  int value;
  promise_type(int k)  : throw_control(k), value(-373)
  {  PRINTF ("Created Promise with %d\n", k);}

  ~promise_type() { PRINT ("Destroyed Promise"); }

  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }
  // This provides the tests for what catches exceptions thrown at
  // different points in the initial await expression.
  auto initial_suspend () {
    PRINT ("get initial_suspend (always)");
    return suspend_always_susp_throws_prt(throw_control);
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
  int get_value (void) { return value; }

  void unhandled_exception() {
    PRINT ("unhandled_exception: caught one!");
    gX = -11;
    // returning from here should end up in final_suspend.
    }
  };
};

// This doesn't have to do much - we only need to exercise the initial
// await expression.

struct coro1
n4849_ia_thrower (int k)
{
  int caught = 0;
  PRINT ("f: about to return 22");
  co_return 22;
}

int main ()
{
  {
  /* Case 0 - nothing should throw.  */
  struct coro1 x0;
  try {
    x0 = n4849_ia_thrower (0);
  } catch (...) {
    PRINT ("main: case 0 ctor threw?");
    abort ();
  }
  /* Resume the initial suspend expression.  */
  PRINT ("main: got coro, resuming..");
  x0.handle.resume();
  int y = x0.handle.promise().get_value();
  if ( y != 22 )
    {
      PRINT ("main: case 0 got the wrong answer.");
      abort ();
    }
  if (!x0.handle.done())
    {
      PRINT ("main: case 0 not done.");
      abort ();
    }
  if (gX != 0)
    {
      PRINT ("main: case 0 body threw?");
      abort ();
    }
  }

  {
  /* Case 1 - initial suspend should throw and thus be caught by the 
     ramp's caller.  */
  struct coro1 x1;
  try {
    x1 = n4849_ia_thrower (1);
  } catch (int message) {
    PRINTF ("main: caught an int %d\n", message);
    if (message != 42)
      {
        PRINT ("main: unexpected value?");
        abort ();
      }
  } catch (...) {
    PRINT ("main: case 1 ctor threw something else?");
    abort ();
  }
  if (gX != 0)
    {
      PRINT ("main: case 0 body threw (how?)");
      abort ();
    }
  }

  {
  /* Case 2 - the await_resume from the initial await expression throws
     this should be caught by the regular function body wrapper.  */
  struct coro1 x2;
  try {
    x2 = n4849_ia_thrower (2);
  } catch (...) {
    PRINT ("main: case 2 ctor threw?");
    abort ();
  }
  // We now resume - and expect the await_resume to throw which should
  // be caught by unhandled_exception().
  PRINT ("main: got coro, resuming..");
  x2.handle.resume();
  int y = x2.handle.promise().get_value();
  if ( y != -373 )
    {
      PRINT ("main: case 2 got the wrong answer.");
      abort ();
    }
  if (!x2.handle.done())
    {
      PRINT ("main: case 2 not done.");
      abort ();
    }
  if (gX != -11)
    {
      PRINT ("main: n4849_is_thrower await_resume exception not caught");
      abort ();
    }
  }
  PRINT ("main: returning");
  return 0;
}
