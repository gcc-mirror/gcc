// PR c++/92666
// { dg-do compile }
// { dg-options "-Wunused-but-set-variable" }

int bar (int, ...);
#if __cplusplus >= 201103L
enum class E : int { F = 0, G = 1 };
#endif
struct S { int s; };

void
foo ()
{
  float r = 1.0f;			// { dg-bogus "set but not used" }
  int i = 2;				// { dg-bogus "set but not used" }
#if __cplusplus >= 201103L
  decltype(nullptr) n = nullptr;	// { dg-bogus "set but not used" }
  E e = E::F;				// { dg-bogus "set but not used" }
#else
  void *n = (void *) 0;
  int e = 4;
#endif
  S s = { 3 };				// { dg-bogus "set but not used" }
  bar (0, r, i, n, e, s);
}
