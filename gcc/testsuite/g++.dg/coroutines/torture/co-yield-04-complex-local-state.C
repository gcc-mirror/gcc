//  { dg-do run }

// using non-trivial types in the coro.

# include "../coro.h"

#include <vector>
#include <string>

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
    PRINTF ("return_value () %s\n",  v.c_str());
    value = v;
  }

  auto yield_value (T v) {
    PRINTF ("yield_value () %s and suspend always\n", v.c_str());
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

int gX ;

struct mycounter 
{ 
  mycounter () : v(0) { PRINT ("mycounter CTOR"); }
  ~mycounter () { gX = 6174; PRINT ("mycounter DTOR"); }
  int value () { return v; }
  void incr () { v++; }
  int v;
};

template <typename T> 
looper<T> with_ctorable_state (std::vector<T> d) noexcept
{
  std::vector<T> loc;
  unsigned lim = d.size()-1;
  mycounter c;
  for (unsigned  i = 0; i < lim ; ++i)
    {
      loc.push_back(d[i]);
      c.incr();
      PRINTF ("f: about to yield value %d \n", i);
      co_yield loc[i];
     }
  loc.push_back(d[lim]);

  PRINT ("f: done");
  co_return loc[lim];
}

int main ()
{
  PRINT ("main: create looper");
  std::vector<std::string> input = {"first", "the", "quick", "reddish", "fox", "done" };
  auto f_coro = with_ctorable_state<std::string> (input);

  PRINT ("main: got looper - resuming (1)");
  if (f_coro.handle.done())
    abort();

  f_coro.handle.resume();
  std::string s = f_coro.handle.promise().get_value();
  if ( s != "first" )
    abort ();

  PRINTF ("main: got : %s\n", s.c_str());
  unsigned check = 1;
  do {
    f_coro.handle.resume();
    s = f_coro.handle.promise().get_value();
    if (s != input[check++])
      abort ();  
    PRINTF ("main: got : %s\n", s.c_str());
  } while (!f_coro.handle.done());

  if ( s != "done" )
    abort ();

  PRINT ("main: should be done");
  if (!f_coro.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }

  if (gX != 6174)
    {
      PRINT ("main: apparently we didn't run mycounter DTOR...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
