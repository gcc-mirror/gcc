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
    int x;
    bool await_ready() const noexcept { return false; }
    void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
    void await_resume() const noexcept {PRINT ("susp-always-resume");}
  };

  /* This returns an int.  */
  struct suspend_always_intprt {
    int x;
    suspend_always_intprt() : x(5) { PRINT ("suspend_always_intprt def ctor"); }
    suspend_always_intprt(int _x) : x(_x) { PRINTF ("suspend_always_intprt ctor with %d\n", x); }
    ~suspend_always_intprt() {}
    bool await_ready() const noexcept { return false; }
    void await_suspend(coro::coroutine_handle<>) const noexcept { PRINT ("susp-always-susp-int");}
    int await_resume() const noexcept { PRINT ("susp-always-resume-int"); return x;}
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

  auto await_transform (int v) {
    PRINTF ("await_transform an int () %d\n",v);
    return suspend_always_intprt (v);
  }

  int get_value () { return value; }
  // Placeholder to satisfy parser, not doing exceptions yet.
  void unhandled_exception() {  /*exit(1);*/ }
  };
};

/* Valued with an await_transform.  */
int gX = 1;
int y = 30;
coro1 f ()
{
  if (gX < 12) {
    gX += y;
    gX += co_await 11;
  } else
    gX += co_await 12;
    
  co_return gX;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  PRINT ("main: got coro1 - checking gX");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      // abort ();
    }
  PRINT ("main: gX OK -- looping");
  do {
    //PRINTF ("main: gX : %d \n", gX);
    f_coro.handle.resume();
  } while (!f_coro.handle.done());
  int y = f_coro.handle.promise().get_value();
  if (y != 42)
    {
      PRINTF ("main: y is wrong : %d, should be 42\n", y);
      //abort ();
    }
  puts ("main: done");
  return 0;
}
