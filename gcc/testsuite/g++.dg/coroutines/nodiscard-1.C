#include <coroutine>
/* Make sure that we correctly warn for unused co_await results.  */

struct task
{
  struct promise_type
  {
    std::suspend_never initial_suspend () noexcept;
    std::suspend_never final_suspend () noexcept;
    void return_void ();
    void unhandled_exception ();
    task get_return_object ();
  };
};

template<typename T>
struct nodiscardable : std::suspend_never
{ [[nodiscard]] T await_resume (); };

template<typename T>
struct discardable : std::suspend_never
{ T await_resume (); };

task
thing ()
{
  co_await nodiscardable<int&>{}; /* { dg-warning "attribute 'nodiscard'" }  */
  co_await nodiscardable<int>{}; /* { dg-warning "attribute 'nodiscard'" }  */

  (void) co_await nodiscardable<int&>{};
  (void) co_await nodiscardable<int>{};

  co_await discardable<int&>{};
  co_await discardable<volatile int&>{};
  /* { dg-warning "implicit dereference will not access" "" { target *-*-* } {.-1} }  */
}

template<typename>
task
other_thing ()
{
  co_await nodiscardable<int&>{}; /* { dg-warning "attribute 'nodiscard'" }  */
  co_await nodiscardable<int>{}; /* { dg-warning "attribute 'nodiscard'" }  */

  (void) co_await nodiscardable<int&>{};
  (void) co_await nodiscardable<int>{};

  co_await discardable<int&>{};
  co_await discardable<volatile int&>{};
  /* { dg-warning "implicit dereference will not access" "" { target *-*-* } {.-1} }  */
}

void
other_thing_caller ()
{
  other_thing <int> ();
}

task
yet_another_thing (auto)
{
  co_await nodiscardable<int&>{}; /* { dg-warning "attribute 'nodiscard'" }  */
  co_await nodiscardable<int>{}; /* { dg-warning "attribute 'nodiscard'" }  */

  (void) co_await nodiscardable<int&>{};
  (void) co_await nodiscardable<int>{};

  co_await discardable<int&>{};
  co_await discardable<volatile int&>{};
  /* { dg-warning "implicit dereference will not access" "" { target *-*-* } {.-1} }  */
}

void
yet_another_thing_caller ()
{
  yet_another_thing (1);
}
