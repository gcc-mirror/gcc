//  { dg-do run }

// Check that we correctly operate when the coroutine object is templated.

#include "../coro.h"

template <typename T> 
struct coro1 {
  struct promise_type;
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  handle_type handle;
  coro1 () : handle(0) {}
  coro1 (handle_type _handle)
    : handle(_handle) {
        PRINT ("Created coro1 object from handle");
  }
  coro1 (const coro1 &) = delete; // no copying
  coro1 (coro1 &&s) : handle(s.handle) {
    s.handle = nullptr;
    PRINT ("Moved coro1");
  }
  coro1 &operator = (coro1 &&s) {
    handle = s.handle;
    s.handle = nullptr;
    return *this;
  }
  ~coro1() {
    PRINT ("Destroyed coro1");
    if ( handle )
      handle.destroy();
  }

  struct suspend_never_prt {
    ~suspend_never_prt() {}
    bool await_ready() const noexcept { return true; }
    void await_suspend(handle_type h) const noexcept { PRINT ("susp-never-susp");}
    void await_resume() const noexcept {PRINT ("susp-never-resume");}
  };

  struct  suspend_always_prt {
    T x;
    bool await_ready() const noexcept { return false; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
    void await_resume() const noexcept {PRINT ("susp-always-resume");}
  };

  /* This returns the int it was constructed with.  */
  struct suspend_always_intprt {
    T x;
    suspend_always_intprt() : x((T)5) { PRINT ("suspend_always_intprt def ctor"); }
    suspend_always_intprt(T _x) : x(_x)
      { PRINTF ("suspend_always_intprt ctor with %ld\n", (long)x); }
    ~suspend_always_intprt() {}
    bool await_ready() const noexcept { return false; }
    void await_suspend(coro::coroutine_handle<>) const noexcept { PRINT ("susp-always-susp-int");}
    int await_resume() const noexcept { PRINT ("susp-always-resume-int"); return x;}
  };

  struct promise_type {
  T value;
  promise_type()  { PRINT ("Created Promise"); }
  ~promise_type() { PRINT ("Destroyed Promise"); }

  coro1 get_return_object() {
    PRINT ("get_return_object: from handle from promise");
    return coro1 (handle_type::from_promise (*this));
  }

  auto initial_suspend() {
    PRINT ("get initial_suspend ");
    return suspend_never_prt{};
  }

  auto final_suspend() noexcept {
    PRINT ("get final_suspend");
    return suspend_always_prt{};
  }

  void return_value (int v) {
    PRINTF ("return_value () %ld\n", (long) v);
    value = v;
  }

  auto await_transform (T v) {
    PRINTF ("await_transform a T () %ld\n", (long)v);
    return suspend_always_intprt (v);
  }

  T get_value () { return value; }
  void unhandled_exception() { PRINT ("** unhandled exception"); }
  };
};

/* Valued with an await_transform.  */
int gX = 2;

template <typename T> 
coro1<T> f ()
{
  for (int i = 0; i < 4; ++i)
    {
      gX += co_await 10;
    }
  co_return gX;
}

int main ()
{
  PRINT ("main: create coro1");
  auto f_coro = f<int>();
  
  PRINT ("main: got coro1 - checking gX");
  if (gX != 2)
    {
      PRINTF ("main: gX is wrong : %d, should be 2\n", gX);
      abort ();
    }
  PRINT ("main: gX OK -- looping");
  do {
    f_coro.handle.resume();
  } while (!f_coro.handle.done());

  int y = f_coro.handle.promise().get_value();

  if (y != 42)
    {
      PRINTF ("main: y is wrong : %d, should be 42\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
