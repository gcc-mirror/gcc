//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test co_yield in templated code.

#include "../coro.h"

template <typename T> 
struct looper {

  struct promise_type {
  T value;
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

  auto final_suspend () noexcept {
    PRINT ("get final_suspend (always)");
    return suspend_always_prt{};
  }

  void return_value (T v) {
    PRINTF ("return_value () %lf\n", (double)v);
    value = v;
  }

  auto yield_value (T v) {
    PRINTF ("yield_value () %lf and suspend always\n", (double)v);
    value = v;
    return suspend_always_prt{};
  }
  
  T get_value (void) { return value; }

  void unhandled_exception() { PRINT ("** unhandled exception"); }
  };
  
  using handle_type = coro::coroutine_handle<looper::promise_type>;
  handle_type handle;

  looper () : handle(0) {}
  looper (handle_type _handle)
    : handle(_handle) {
        PRINT("Created coro1 object from handle");
  }
  looper (const looper &) = delete; // no copying
  looper (looper &&s) : handle(s.handle) {
    s.handle = nullptr;
    PRINT("looper mv ctor ");
  }
  looper &operator = (looper &&s) {
    handle = s.handle;
    s.handle = nullptr;
    PRINT("looper op=  ");
    return *this;
  }
  ~looper() {
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

};

// Contrived to avoid non-scalar state across the yield.
template <typename T> 
looper<T> f () noexcept
{
  for (int i = 5; i < 10 ; ++i)
    {
      PRINTF ("f: about to yield %d\n", i);
      co_yield (T) i;
    }

  PRINT ("f: about to return 6174");
  co_return 6174;
}

// contrived, only going to work for an int.
int main ()
{
  PRINT ("main: create int looper");
  auto f_coro = f<int> ();

  if (f_coro.handle.done())
    {
      PRINT ("main: said we were done, but we hadn't started!");
      abort();
    }

  PRINT ("main: OK -- looping");
  int y, test = 5;
  do {
    f_coro.handle.resume();
    if (f_coro.handle.done())
      break;
    y = f_coro.handle.promise().get_value();
    if (y != test)
      {
	PRINTF ("main: failed for test %d, got %d\n", test, y);
	abort();
      }
    test++;
  } while (test < 20);

  y = f_coro.handle.promise().get_value();
  if ( y != 6174 )
    abort ();

  PRINT ("main: apparently got 6174");
  if (!f_coro.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
