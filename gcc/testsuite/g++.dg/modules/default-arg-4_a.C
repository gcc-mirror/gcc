// C++20 P1766R1 - Mitigating minor modules maladies
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

int foo (int i = 42);
template <typename T, typename U = int>
int bar ();
template <typename T, int N = 42>
int baz ();

export module M;

export inline int
qux ()
{
  return foo () + bar <int> () + baz <int> ();
}

export using ::foo;
export using ::bar;
export using ::baz;
