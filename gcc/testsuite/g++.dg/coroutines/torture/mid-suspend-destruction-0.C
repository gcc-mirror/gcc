// { dg-do run }
// { dg-output "main: returning(\n|\r\n|\r)" }
// { dg-output "Destroyed coro1(\n|\r\n|\r)" }
// { dg-output "Destroyed suspend_always_prt(\n|\r\n|\r)" }
// { dg-output "Destroyed Promise(\n|\r\n|\r)" }

// Check that we still get the right DTORs run when we let a suspended coro
// go out of scope.

#include "../coro.h"

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
        printf ("Destroyed coro1\n");
        if ( handle )
          handle.destroy();
  }

  struct suspend_never_prt {
  bool await_ready() const noexcept { return true; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-never-susp");}
  void await_resume() const noexcept { PRINT ("susp-never-resume");}
  ~suspend_never_prt() {};
  };

  struct  suspend_always_prt {
  bool await_ready() const noexcept { return false; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
  void await_resume() const noexcept { PRINT ("susp-always-resume");}
  ~suspend_always_prt() { printf ("Destroyed suspend_always_prt\n"); }
  };

  struct promise_type {
  promise_type() {  PRINT ("Created Promise"); }
  ~promise_type() { printf ("Destroyed Promise\n"); }

  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }
  auto initial_suspend () {
    PRINT ("get initial_suspend (always)");
    return suspend_always_prt{};
  }
  auto final_suspend () noexcept {
    PRINT ("get final_suspend (always)");
    return suspend_always_prt{};
  }
  void return_void () {
    PRINT ("return_void ()");
  }

  void unhandled_exception() { PRINT ("** unhandled exception"); }
  };
};

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    {
      PRINT ("main: f() should be suspended, says it's done");
      abort();
    }

#if __has_builtin (__builtin_coro_suspended)
  if (! __builtin_coro_suspended(handle))
    {
      PRINT ("main: f() should be suspended, but says it isn't");
      abort();
    }
#endif

  /* We are suspended... so let everything out of scope and therefore
     destroy it.  */

  puts ("main: returning");
  return 0;
}
