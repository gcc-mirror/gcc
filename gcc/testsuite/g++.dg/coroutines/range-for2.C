// PR c++/118574
// { dg-do run }
// { dg-additional-options "-std=c++23 -O2" }

#include <coroutine>

[[gnu::noipa]] void
baz (int *)
{
}

struct D {
  D () : d (new int (42)) {}
  ~D () { if (*d != 42) __builtin_abort (); *d = 0; baz (d); delete d; }
  int *d;
};

struct E {
  E (const D &x) : e (x) {}
  void test () const { if (*e.d != 42) __builtin_abort (); }
  ~E () { test (); }
  const D &e;
};

struct A {
  const char **a = nullptr;
  int n = 0;
  const E *e1 = nullptr;
  const E *e2 = nullptr;
  void test () const { if (e1) e1->test (); if (e2) e2->test (); }
  void push_back (const char *x) { test (); if (!a) a = new const char *[2]; a[n++] = x; }
  const char **begin () const { test (); return a; }
  const char **end () const { test (); return a + n; }
  ~A () { test (); delete[] a; }
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
foo (const E &e1, const E &e2)
{
  A a;
  a.e1 = &e1;
  a.e2 = &e2;
  a.push_back ("foo");
  a.push_back ("bar");
  return a;
}

C
bar ()
{
  A ret;
  for (const auto &item : foo (E{D {}}, E{D {}}))
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
