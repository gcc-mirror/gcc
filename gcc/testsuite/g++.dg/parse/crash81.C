// PR c++/120876
// { dg-do compile { target c++11 } }

template <typename T>
struct S {
  static bool foo (decltype (bar (T {})));	// { dg-error "'bar' was not declared in this scope; did you mean 'baz'\\\?" }
  static constexpr bool s = foo (0);		// { dg-error "declaration of 'S<int>::foo' depends on itself" }
};

void
baz ()
{
  S <int>::s;
}
