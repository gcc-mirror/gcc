//  { dg-additional-options "-fsyntax-only -fexceptions -w" }

// Diagose bad traits traits : fake something faulty.

namespace std {
  // name is present, but not a template.
  struct coroutine_traits {
  };
}

int
bad_coroutine (void)
{
  co_yield 5; // { dg-error {coroutines require a traits template; cannot find 'std::coroutine_traits'} }
  co_return;
}
