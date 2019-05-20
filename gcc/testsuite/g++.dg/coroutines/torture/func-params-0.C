//  { dg-do run }
#if __clang__
# include <experimental/coroutine>
# include <utility>
namespace coro = std::experimental::coroutines_v1;
#else
# include "../coro.h"
namespace coro = std::experimental::coroutines_n4775;
#endif

// Test promise construction from function args list.
// We will use the promise to contain this to avoid having to include
// additional C++ headers.

/* just to avoid cluttering dump files. */
extern "C" int puts (const char *);
extern "C" int printf (const char *, ...);
extern "C" void abort (void) __attribute__((__noreturn__));

#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF (void)
#else
#  define PRINT(X) puts(X)
#  define PRINTF printf
#endif

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
  promise_type(int x) : value (x) {  PRINTF ("Created Promise with %d\n", x); }
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
  int get_value (void) { return value; }
  // Placeholder to satisfy parser, not doing exceptions yet.
  void unhandled_exception() {  /*exit(1);*/ }
  };
};

inline
struct coro1 f (int x) noexcept
{
  PRINT ("coro1: about to return");
  co_return 42;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f (555);
  int y = x.handle.promise().get_value();
  if ( y != 555 )
    abort ();
  PRINTF ("main: after coro1 ctor %d - now resuming\n", y);
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
      //x.handle.resume();
    }
  PRINT ("main: returning");
  return 0;
}
