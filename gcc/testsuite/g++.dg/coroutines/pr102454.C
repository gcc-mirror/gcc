//  { dg-additional-options "-fno-exceptions" }
//  { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <coroutine>
#include <string>

template <typename T>
struct looper {
  struct promise_type {
    auto get_return_object () { return handle_type::from_promise (*this); }
    auto initial_suspend () { return suspend_always_prt {}; }
    auto final_suspend () noexcept { return suspend_always_prt {}; }
    void return_value (T);
    void unhandled_exception ();
  };

  using handle_type = std::coroutine_handle<promise_type>;

  looper (handle_type);

  struct suspend_always_prt {
    bool await_ready () noexcept;
    void await_suspend (handle_type) noexcept;
    void await_resume () noexcept;
  };
};

template <typename T>
looper<T>
with_ctorable_state (T)
{
  co_return T ();
}

auto
foo ()
{
  return with_ctorable_state<std::string>;
}
