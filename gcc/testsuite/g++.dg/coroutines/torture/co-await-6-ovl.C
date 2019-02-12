#if __clang__
# include <experimental/coroutine>
namespace coro = std::experimental::coroutines_v1;
#else
# include "../coro.h"
namespace coro = std::experimental::coroutines_n4775;
#endif

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
    PRINT("Moved coro1");
  }
  coro1 &operator = (coro1 &&s) {
    handle = s.handle;
    s.handle = nullptr;
    return *this;
  }
  ~coro1() {
    PRINT("Destroyed coro1");
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
    int x;
    bool await_ready() const noexcept { return false; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
    void await_resume() const noexcept {PRINT ("susp-always-resume");}
  };

  /* This returns an int.  */
  struct suspend_always_intprt {
    int x;
    suspend_always_intprt() : x(5) {}
    ~suspend_always_intprt() {}
    bool await_ready() const noexcept { return false; }
    void await_suspend(coro::coroutine_handle<>) const noexcept { puts ("susp-always-susp-int");}
    int await_resume() const noexcept {puts ("susp-always-resume-int"); return x;}
  };

  struct promise_type {
  int value;
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

  auto final_suspend() {
    PRINT ("get final_suspend");
    return suspend_always_prt{};
  }

  void return_value (int v) {
    PRINTF ("return_value () %d\n",v);
    value = v;
  }

  int get_value () { return value; }
  // Placeholder to satisfy parser, not doing exceptions yet.
  void unhandled_exception() {  /*exit(1);*/ }
  };

  struct empty {
    auto operator co_await() const noexcept { 
      return suspend_always_prt{};
    }
  };
};


/* The simplest valued co_await we can do.  */
int gX = 1;
coro1::empty e{};

coro1 f ()
{
  co_await (e); /* overload */
  co_return gX + 10;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  PRINT ("main: got coro1 - checking gX");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      abort ();
    }
  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }
  PRINT ("main: resuming [1]");
  f_coro.handle.resume();
  /* we should now have returned with the co_return (15) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 11)
    {
      PRINTF ("main: y is wrong : %d, should be 11\n", y);
      abort ();
    }
  puts ("main: done");
  return 0;
}
