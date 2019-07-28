//  { dg-do run }

#if __clang__
# include <experimental/coroutine>
# include <utility>
namespace coro = std::experimental::coroutines_v1;
#else
# include "../coro.h"
namespace coro = std::experimental::coroutines_n4775;
#endif

// check the code-gen for the failed alloc return.

// avoid including headers, where possible.
#if 0 && __has_include(<new>)
#  include <new>
#else

// Required when get_return_object_on_allocation_failure() is defined by
// the promise.
// we need a no-throw new, and new etc.  build the relevant pieces here to
// avoid needing the headers in the test.

namespace std {
  struct nothrow_t {};
  constexpr nothrow_t nothrow = {};
  typedef __SIZE_TYPE__ size_t;
} // end namespace std

void* operator new(std::size_t, const std::nothrow_t&) noexcept;
void  operator delete(void* __p, const std::nothrow_t&) noexcept;
#endif

/* just to avoid cluttering dump files. */
extern "C" int puts (const char *);
extern "C" int printf (const char *, ...);
extern "C" void abort (void) __attribute__((__noreturn__));

#ifndef OUTPUT
#  define PRINT(X)
#else
#  define PRINT(X) puts(X)
#endif

struct coro1 {
  struct promise_type;
  using handle_type = coro::coroutine_handle<coro1::promise_type>;
  handle_type handle;
  coro1 () noexcept : handle(0) {}
  coro1 (handle_type _handle) noexcept
    : handle(_handle)  {
        PRINT("Created coro1 object from handle");
  }
  coro1 (const coro1 &) = delete; // no copying
  coro1 (coro1 &&s) noexcept : handle(s.handle)  {
	s.handle = nullptr;
	PRINT("coro1 mv ctor ");
  }
  coro1 &operator = (coro1 &&s) noexcept {
	handle = s.handle;
	s.handle = nullptr;
	PRINT("coro1 op=  ");
	return *this;
  }
  ~coro1() noexcept {
        PRINT("Destroyed coro1");
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
  };

  struct promise_type {
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
  void return_void () {
    PRINT ("return_void ()");
  }
  // Placeholder to satisfy parser, not doing exceptions yet.
  void unhandled_exception() {  /*exit(1);*/ }
  static coro1 get_return_object_on_allocation_failure () noexcept;
  }; // promise
}; // coro1

coro1 coro1::promise_type::
get_return_object_on_allocation_failure () noexcept {
  PRINT ("alloc fail return");
  return coro1 (nullptr);
}

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
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
      //x.handle.resume();
    }
  PRINT ("main: returning");
  return 0;
}
