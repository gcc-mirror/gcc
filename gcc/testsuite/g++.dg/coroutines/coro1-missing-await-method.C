//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

#define MISSING_AWAIT_READY
#define MISSING_AWAIT_SUSPEND
#define MISSING_AWAIT_RESUME
#include "coro1-ret-int-yield-int.h"

coro1
bar0 ()
{
  co_await coro1::suspend_never_prt{}; // { dg-error {no member named 'await_ready' in 'coro1::suspend_never_prt'} }
  co_yield 5; // { dg-error {no member named 'await_suspend' in 'coro1::suspend_always_prt'} }
  co_await coro1::suspend_always_intprt(5); // { dg-error {no member named 'await_resume' in 'coro1::suspend_always_intprt'} }
  co_return 0;
}

int main (int ac, char *av[]) {
  struct coro1 x0 = bar0 ();
  return 0;
}
