//  { dg-additional-options "-fsyntax-only -fexceptions -w" }

// Diagose missing traits (e.g. fail to include <coroutine>).

int
bad_coroutine (void)
{
  co_yield 5; // { dg-error {coroutines require a traits template; cannot find 'std::coroutine_traits'} }
  co_return;
}
