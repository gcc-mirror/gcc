// { dg-do run }
// { dg-additional-options "-std=c++20 -fcontracts -fcontract-continuation-mode=on -fcontracts-nonattr" }
#include <coroutine>

#ifndef OUTPUT
#  define PRINT(X)
#  define PRINTF(...)
#else
#include <stdio.h>
#  define PRINT(X) puts(X)
#  define PRINTF(...) printf(__VA_ARGS__)
#endif

struct coro1 {
  struct promise_type;
  using handle_type = std::coroutine_handle<coro1::promise_type>;
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

  struct  suspend_always_prt {
  bool await_ready() const noexcept { return false; }
  void await_suspend(handle_type) const noexcept { PRINT ("susp-always-susp");}
  void await_resume() const noexcept { PRINT ("susp-always-resume");}
  ~suspend_always_prt() { PRINT ("susp-always-dtor"); }
  };

  struct promise_type {

  promise_type() : vv(-1) {  PRINT ("Created Promise"); }
  promise_type(int __x) : vv(__x) {  PRINTF ("Created Promise with %d\n",__x); }
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

  void return_value (int v) {
    PRINTF ("return_value (%d)\n", v);
    vv = v;
  }

  void unhandled_exception() { PRINT ("** unhandled exception"); }

  int get_value () { return vv; }
  private:
    int vv;
  };
};

struct Base {
  virtual struct coro1
  f (const int v) noexcept [[pre: v == 41]] 
  {
    PRINT ("Base: about to return");
    co_return v;
   }
};

struct Derived : Base {
  virtual struct coro1
  f (const int v) noexcept [[pre: v == 6171]] override 
  {
    PRINT ("Derived: about to return");
    co_return v;
   }

};

int main ()
{
  PRINT ("main: create coro1");
  Base b;
  struct coro1 x = b.f (42);
  x.handle.resume();
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    __builtin_abort ();

  Derived d;
  struct coro1 z = d.f (6174);
  z.handle.resume();
  y = z.handle.promise().get_value();
  if ( y != 6174 )
    __builtin_abort ();

  struct coro1 c = d.Base::f (42);
  c.handle.resume();
  y = c.handle.promise().get_value();
  if ( y != 42 )
    __builtin_abort ();
  return 0;
}
