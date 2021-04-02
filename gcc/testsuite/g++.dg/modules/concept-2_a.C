// { dg-additional-options "-fmodules-ts -fconcepts" }

export module foo;
// { dg-module-cmi foo }

export template<typename T>
requires (sizeof (T) == 1)
  char f1 (T x) { return 0; }

export template<typename T>
requires (sizeof (T) != 1)
  int f1 (T x) { return 0; }

void foo (int i, char c)
{
  static_assert (sizeof (f1 (i)) == sizeof (int));
  static_assert (sizeof (f1 (c)) == sizeof (char));
}
