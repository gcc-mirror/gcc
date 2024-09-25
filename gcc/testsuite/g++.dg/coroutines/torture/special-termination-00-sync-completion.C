// { dg-do run }
#include <coroutine>

#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF(...)
#else
#include <stdio.h>
#  define PRINT(X) puts(X)
#  define PRINTF(...) printf(__VA_ARGS__)
#endif

// coroutine management object with simple interlocks for the underlying
// coroutine.

struct coro1 {
  struct promise_type;
  using handle_type = std::coroutine_handle<coro1::promise_type>;

  handle_type handle = nullptr;

  coro1 () : handle(0) {}
  coro1 (handle_type _handle)
    : handle(_handle)
  {
    PRINT("Created coro1 object from handle");
    // We're alive - let the promise object know so that it can notify us
    // if the coroutine terminates before this object is destroyed.
    handle.promise().set_owner (this);
  }

  coro1 (const coro1 &) = delete; // no copying

  coro1 (coro1 &&s) : handle (s.handle) {
    s.handle = nullptr;
    PRINT("coro1 mv ctor ");
    handle.promise().set_owner (this);
  }

  coro1 &operator = (coro1 &&s) {
    handle = s.handle;
    s.handle = nullptr;
    PRINT("coro1 op=  ");
    handle.promise().set_owner (this);
    return *this;
  }

  ~coro1() {
    PRINT("Destroyed coro1");
    // We might come here before the coroutine has finished so...
    if ( handle )
      {
        handle.promise().set_owner (nullptr);
        handle.destroy();
      }
  }

  // Special awaiters.
  struct suspend_always_prt {
  bool await_ready() const noexcept { PRINT ("susp-always-is-not-ready") ; return false; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
  void await_resume() const noexcept { PRINT ("susp-always-resume");}
  ~suspend_always_prt() { PRINT ("susp-always-dtor"); }
  };

  struct suspend_never_prt {
  bool await_ready () const noexcept { PRINT ("susp-never-is-ready") ; return true; }
  void await_suspend (handle_type) const noexcept { PRINT ("susp-never-susp");}
  void await_resume () const noexcept { PRINT ("susp-never-resume");}
  ~suspend_never_prt () { PRINT ("susp-never-dtor"); }
  };

  struct promise_type {

  promise_type () : vv(-1) {  PRINT ("Created Promise"); }

  promise_type (int __x) : vv(__x) {  PRINTF ("Created Promise with %d\n",__x); }

  ~promise_type () {
  PRINT ("Destroyed Promise");
  // The coroutine is about to become invalid - so remove the handle from the
  // owner.
  if (owner)
    owner->handle = nullptr;
  }

  // The coro1 ramp return object will be constructed from this.
  auto get_return_object () {
    PRINT ("get_return_object: handle from promise");
    return handle_type::from_promise (*this);
  }

  auto initial_suspend () { return suspend_never_prt{}; }
  auto final_suspend () noexcept { return suspend_never_prt{}; }

  void return_value (int v) {
    PRINTF ("return_value (%d)\n", v);
    vv = v;
  }

  void unhandled_exception() { PRINT ("** unhandled exception"); }

  int get_value () { return vv; }
  void set_owner (coro1 *new_owner) { owner = new_owner; }

  private:
    coro1 *owner = nullptr;
    int vv;
  };
};

struct coro1
finishes_synchronously (const int v)
{
  co_return v;
}

int main ()
{
  struct coro1 x = finishes_synchronously (42);
  // the underlying coroutine is done and destroyed... and we should have
  // signalled that by clearing coro1's handle.
  if (x.handle)
    __builtin_abort ();
  // ... so we just clean up the return object here.
  return 0;
}
