// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }

#include <coroutine>
#include <cstdio>

#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF(X,...)
#else
#  define PRINT(X) puts(X)
#  define PRINTF printf
#endif

struct future {
  struct promise_type;
  using handle_type = std::coroutine_handle<future::promise_type>;
  handle_type handle;
  future () : handle(0) {}
  future (handle_type _handle)
    : handle(_handle) {
        PRINT("Created future object from handle");
  }
  future (const future &) = delete; // no copying
  future (future &&s) : handle(s.handle) {
	s.handle = nullptr;
	PRINT("future mv ctor ");
  }
  future &operator = (future &&s) {
	handle = s.handle;
	s.handle = nullptr;
	PRINT("future op=  ");
	return *this;
  }
  ~future() {
        PRINT("Destroyed future");
        if ( handle )
          handle.destroy();
  }

  struct promise_type {
    void return_value (int v) {
      PRINTF ("return_value (%d)\n", v);
      vv = v;
    }

    std::suspend_always initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    auto get_return_object() {return handle_type::from_promise (*this);}
    
    int get_value () { return vv; }
  private:
    int vv;
  };
  bool await_ready() { return false; }
  void await_suspend(std::coroutine_handle<>) {}
  void await_resume() {}
};

extern "C" future
test () {
  co_return 22;
}

extern "C" future
f () noexcept
{
  PRINT ("future: about to return");
  co_return 42;
}

int main ()
{
  PRINT ("main: create future");
  future x = f ();
  PRINT ("main: got future - resuming");
  if (x.handle.done())
    __builtin_abort ();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    __builtin_abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      __builtin_abort ();
    }
  PRINT ("main: returning");
  return 0;
}
