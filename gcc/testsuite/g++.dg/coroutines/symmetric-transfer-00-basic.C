// See PR94359, we will need either a general solution to this, or at least
// some hook for targets to opt in, for now the test will work on targets that
// can do the tailcall (which would normally be available for O2+)

// { dg-do run { target { i?86-*-linux-gnu x86_64-*-linux-gnu *-*-darwin* } } }
// { dg-additional-options "-O2" }

#if __has_include(<coroutine>)

#include <coroutine>
namespace coro = std;

#elif __has_include(<experimental/coroutine>)

#include <experimental/coroutine>
namespace coro = std::experimental;

#endif

#if __clang__
#  include <utility>
#endif

#include <chrono>
#include <thread>
#include <cstdio>

template <typename T> 
struct Loopy {
  struct promise_type;
  using handle_type = coro::coroutine_handle<Loopy::promise_type>;
  handle_type handle;

  struct promise_type {
    // Cause the Loopy object to be created from the handle.
    auto get_return_object () {
     return handle_type::from_promise (*this);
    }

    coro::suspend_never yield_value(T value) {
      //fprintf (stderr, "%s yields %d\n", me, value);
      current_value = value;
      return coro::suspend_never{};
    }

    coro::suspend_always initial_suspend() { return {}; }
    coro::suspend_always final_suspend() { return {}; }

    void unhandled_exception() { /*std::terminate();*/  }
    void return_void() {}

    void set_peer (handle_type  _p) { peer = _p; }
    void set_me (const char *m) { me = m; }

    T get_value () {return current_value;}
    T current_value;
    handle_type peer;
    const char *me;
  };

  Loopy () : handle(0) {}
  Loopy (handle_type _handle) : handle(_handle) {}
  Loopy (const Loopy &) = delete;
  Loopy (Loopy &&s) : handle(s.handle) { s.handle = nullptr; }
  ~Loopy() { if ( handle ) handle.destroy(); }

  struct loopy_awaiter {
    handle_type p;
    bool await_ready () { return false; }
    /* continue the peer. */
    handle_type await_suspend (handle_type h) {
      p = h.promise().peer;
      return p;
    }
    T await_resume () { return p.promise().get_value (); }
  };
};

Loopy<int>
pingpong (const char *id)
{
  int v = 0;
  Loopy<int>::loopy_awaiter aw{};
  for (;;)
    {
      co_yield (v+1);
      //std::this_thread::sleep_for(std::chrono::milliseconds(500));
      if (v > 10'000'000)
        break;
      v = co_await aw;
      //fprintf (stderr, "%s = %d\n", id, v);
    }
 //fprintf (stderr, "%s = %d\n", id, v);
}

int main ()
{
  // identify for fun.. 
  const char *ping_id = "ping";
  const char *pong_id = "pong";
  auto ping = pingpong (ping_id); // created suspended.
  auto pong = pingpong (pong_id);

  // point them at each other...
  ping.handle.promise().set_peer (pong.handle);
  pong.handle.promise().set_peer (ping.handle);

  ping.handle.promise().set_me (ping_id);
  pong.handle.promise().set_me (pong_id);

  // and start them ...
  ping.handle.resume ();
  pong.handle.resume ();
  
}

