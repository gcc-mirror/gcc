// PR c++/95310
// { dg-do compile { target concepts } }

template <class T>
using iter_reference_t = decltype(*T{});

template <typename F>
struct result { using type = iter_reference_t<F>; };

template <class Out, const int& N>
concept indirectly_writable = requires(Out o) { // { dg-bogus "F =" }
  iter_reference_t<Out>(*o) = N;
};

const int a = 0;
static_assert(indirectly_writable<const int*, a>); // { dg-error "assert" }
