// PR c++/118574
// { dg-do run { target c++20 } }
// { dg-additional-options -frange-for-ext-temps }

#include <coroutine>

struct A {
  const char **a = nullptr;
  int n = 0;
  void push_back (const char *x) { if (!a) a = new const char *[2]; a[n++] = x; }
  const char **begin () const { return a; }
  const char **end () const { return a + n; }
  ~A () { delete[] a; }
};

struct B {
  long ns;
  bool await_ready () const noexcept { return false; }
  void await_suspend (std::coroutine_handle<> h) const noexcept {
    volatile int v = 0;
    while (v < ns)
      v = v + 1;
    h.resume ();
  }
  void await_resume () const noexcept {}
};

struct C {
  struct promise_type {
    const char *value;
    std::suspend_never initial_suspend () { return {}; }
    std::suspend_always final_suspend () noexcept { return {}; }
    void return_value (const char *v) { value = v; }
    void unhandled_exception () { __builtin_abort (); }
    C get_return_object () { return C{this}; }
  };
  promise_type *p;
  explicit C (promise_type *p) : p(p) {}
  const char *get () { return p->value; }
};

A
foo ()
{
  A a;
  a.push_back ("foo");
  a.push_back ("bar");
  return a;
}

C
bar ()
{
  A ret;
  for (const auto &item : foo ())
    {
      co_await B{200000};
      ret.push_back (item);
    }
  co_return "foobar";
}

int
main ()
{
  auto task = bar ();
  if (__builtin_strcmp (task.get (), "foobar"))
    __builtin_abort ();
}
