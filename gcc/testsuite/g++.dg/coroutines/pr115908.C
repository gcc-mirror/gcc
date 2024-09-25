#include <coroutine>

#ifdef OUTPUT
#include <iostream>
#endif

struct Promise;

bool promise_live = false;

struct Handle : std::coroutine_handle<Promise> {
    Handle(Promise &p) : std::coroutine_handle<Promise>(Handle::from_promise(p)) {
        if (!promise_live)
          __builtin_abort ();
#ifdef OUTPUT
        std::cout << "Handle(Promise &)\n";
#endif
    }
    Handle(Promise &&p) : std::coroutine_handle<Promise>(Handle::from_promise(p)) {
        if (!promise_live)
          __builtin_abort ();
#ifdef OUTPUT
        std::cout << "Handle(Promise &&)\n";
#endif
    }

    using promise_type = Promise;
};

struct Promise {
    Promise() {
#ifdef OUTPUT
        std::cout << "Promise()\n";
#endif
        promise_live = true;
    }
    ~Promise() {
#ifdef OUTPUT
        std::cout << "~Promise()\n";
#endif
        if (!promise_live)
          __builtin_abort ();
        promise_live = false;
    }
    Promise& get_return_object() noexcept {
#ifdef OUTPUT
        std::cout << "get_return_object()\n";
#endif
        if (!promise_live)
          __builtin_abort ();
        return *this;
    }
    std::suspend_never initial_suspend() const noexcept { return {}; }
    std::suspend_never final_suspend() const noexcept { return {}; }
    void return_void() const noexcept {
        if (!promise_live)
          __builtin_abort ();
#ifdef OUTPUT
        std::cout << "return_void()\n";
#endif
    }
    void unhandled_exception() const noexcept {}
};

Handle Coro() {
    co_return;
}

int main() {
  Coro();

  if (promise_live)
    __builtin_abort ();
  return 0;
}
