//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// test boolean return from await_suspend ().

#include "../coro.h"

int coro1_dtor_ran = 0;
int promise_dtor_ran = 0;

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
        coro1_dtor_ran++;
        // The coro handle will point to an invalid frame by this stage,
        // the coroutine will already have self-destroyed the frame and
        // promise.
  }

  struct suspend_never_prt {
  bool await_ready() const noexcept { return true; }
  bool await_suspend(handle_type) const noexcept {
    PRINT ("susp-never-susp"); // never executed.
    return true; // ...
  }
  void await_resume() const noexcept {PRINT ("susp-never-resume");}
  ~suspend_never_prt() {};
  };

  struct  suspend_always_prt {
  bool await_ready() const noexcept { return false; }
  bool await_suspend(handle_type) const noexcept {
    PRINT ("susp-always-susp, but we're going to continue.. ");
    return false; // not going to suspend.
  }
  void await_resume() const noexcept { PRINT ("susp-always-resume");}
  };

  struct promise_type {
  promise_type() {  PRINT ("Created Promise"); }
  ~promise_type() {
     PRINT ("Destroyed Promise"); 
     promise_dtor_ran++;
   }

  coro1 get_return_object () {
    PRINT ("get_return_object: from handle from promise");
    return coro1 (handle_type::from_promise (*this));
  }
  auto initial_suspend () {
    PRINT ("get initial_suspend (always, but really never) ");
    return suspend_always_prt{};
  }
  auto final_suspend () noexcept {
    PRINT ("get final_suspend (always, but never) ");
    return suspend_always_prt{};
  }
  void return_void () {
    PRINT ("return_void ()");
  }
  void unhandled_exception() { PRINT ("** unhandled exception"); }
  };
};

struct coro1
my_coro () noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  { // scope so that we can examine the coro dtor marker.
    PRINT ("main: creating coro");

    // This should just run through to completion/destruction.
    // In both the initial and final await expressions, the await_suspend()
    // method will return 'false' and prevent the suspension.
    struct coro1 x = my_coro ();

    PRINT ("main: the coro frame should be already destroyed");
    // We will take the running of the promise DTOR as evidence that the
    // frame was destroyed as expected.
    if (promise_dtor_ran != 1)
      {
	PRINT ("main: apparently we didn't destroy the frame");
	abort ();
      }
  }
  if (coro1_dtor_ran != 1 || promise_dtor_ran != 1)
    {
      PRINT ("main: bad DTOR counts");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
