// { dg-do run }

// With the changes to deal with CWG2563 (and PR119916) we now use the
// referenced promise in the return expression.  It is quite reasonable
// for a body implementation to complete before control is returned to
// the ramp - so we need to ensure that the promise lifetime is extended
// in that case, tested here.

#include <coroutine>

#ifdef OUTPUT
#include <iostream>
#endif

struct Promise;

int promise_life = 0;

struct Handle : std::coroutine_handle<Promise> {

    Handle(Promise &p) : std::coroutine_handle<Promise>(Handle::from_promise(p)) {
#ifdef OUTPUT
        std::cout << "Handle(Promise &) " << promise_life << std::endl;
#endif
         if (promise_life <= 0)
          __builtin_abort ();
   }

    Handle(Promise &&p) : std::coroutine_handle<Promise>(Handle::from_promise(p)) {
#ifdef OUTPUT
        std::cout << "Handle(Promise &&) "  << promise_life  << std::endl;
#endif
         if (promise_life <= 0)
          __builtin_abort ();
   }

    using promise_type = Promise;
};

struct Promise {
    Promise() {
#ifdef OUTPUT
        std::cout << "Promise()" << std::endl;
#endif
        promise_life++;
    }

    Promise(Promise& p){
#ifdef OUTPUT
        std::cout << "Promise(Promise&)" << std::endl;
#endif
        promise_life++;
    }

    ~Promise() {
#ifdef OUTPUT
        std::cout << "~Promise()" << std::endl;
#endif
        if (promise_life <= 0)
          __builtin_abort ();
        promise_life--;
    }

    Promise& get_return_object() noexcept {
#ifdef OUTPUT
        std::cout << "get_return_object() " << promise_life << std::endl;
#endif
        if (promise_life <= 0)
          __builtin_abort ();
        return *this;
    }

    std::suspend_never initial_suspend() const noexcept {
#ifdef OUTPUT
        std::cout << "initial_suspend()" << std::endl;
#endif
     return {}; 
    }
    std::suspend_never final_suspend() const noexcept {
#ifdef OUTPUT
        std::cout << "final_suspend()" << std::endl;
#endif
    return {};
    }
    void return_void() const noexcept {
        if (!promise_life)
          __builtin_abort ();
#ifdef OUTPUT
        std::cout << "return_void()" << std::endl;
#endif
    }
    void unhandled_exception() const noexcept {}
};

Handle Coro() {

#ifdef OUTPUT
        std::cout << "Coro()" << std::endl;
#endif
    co_return;
}

int main() {

  Coro();
#ifdef OUTPUT
        std::cout << "done Coro()" << std::endl;
#endif
  if (promise_life)
    __builtin_abort ();
  return 0;
}
