//  { dg-additional-options "-fsyntax-only -fexceptions -w" }

// Diagose missing coroutine handle class template.

namespace std {
  //  coroutine traits
  template<typename _R, typename...> struct coroutine_traits {
    using promise_type = typename _R::promise_type;
  };

  // name is present, but not a template.
  struct coroutine_handle {
  };
}

int
bad_coroutine (void)
{
  co_yield 5; // { dg-error {coroutines require a handle class template; cannot find 'std::coroutine_handle'} }
  co_return;
}
