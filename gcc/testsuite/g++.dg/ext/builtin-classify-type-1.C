// { dg-do run { target c++11 } }

extern "C" void abort ();

template <int N>
void
foo ()
{
  enum E { E1 } e = E1;
  struct S { int s; } s = { 0 };
  union U { int u; } u = { 0 };
  int a[2] = { 0, 0 };
  bool b = false;
  const char *p = (const char *) 0;
  float f = 0.0;
  _Complex double c = 0.0;
  struct T { void foo (); };
  int &r = a[0];
  int S::*q = &S::s;
  static_assert (__builtin_classify_type (void) == 0, "");
  static_assert (__builtin_classify_type (int) == 1, "");
  static_assert (__builtin_classify_type (enum E) == 3, "");
  static_assert (__builtin_classify_type (bool) == 4, "");
  static_assert (__builtin_classify_type (const char *) == 5, "");
  static_assert (__builtin_classify_type (int &) == 6, "");
  static_assert (__builtin_classify_type (int &&) == 6, "");
  static_assert (__builtin_classify_type (int S::*) == 7, "");
  static_assert (__builtin_classify_type (float) == 8, "");
  static_assert (__builtin_classify_type (_Complex double) == 9, "");
  static_assert (__builtin_classify_type (int (int, int)) == 10, "");
  static_assert (__builtin_classify_type (struct S) == 12, "");
  static_assert (__builtin_classify_type (union U) == 13, "");
  static_assert (__builtin_classify_type (int [2]) == 14, "");
  static_assert (__builtin_classify_type (__typeof__ (a[0])) == 1, "");
  static_assert (__builtin_classify_type (__typeof__ (e)) == 3, "");
  static_assert (__builtin_classify_type (__typeof__ (b)) == 4, "");
  static_assert (__builtin_classify_type (__typeof__ (p)) == 5, "");
  static_assert (__builtin_classify_type (decltype (r)) == 6, "");
  static_assert (__builtin_classify_type (__typeof__ (q)) == 7, "");
  static_assert (__builtin_classify_type (__typeof__ (f)) == 8, "");
  static_assert (__builtin_classify_type (__typeof__ (c)) == 9, "");
  static_assert (__builtin_classify_type (__typeof__ (abort)) == 10, "");
  static_assert (__builtin_classify_type (__typeof__ (s)) == 12, "");
  static_assert (__builtin_classify_type (__typeof__ (u)) == 13, "");
  static_assert (__builtin_classify_type (__typeof__ (a)) == 14, "");
  if (__builtin_classify_type (a[0]) != 1)
    abort ();
  if (__builtin_classify_type (e) != 3)
    abort ();
  if (__builtin_classify_type (b) != 4)
    abort ();
  if (__builtin_classify_type (p) != 5)
    abort ();
  if (__builtin_classify_type (r) != 1)
    abort ();
  if (__builtin_classify_type (q) != 7)
    abort ();
  if (__builtin_classify_type (f) != 8)
    abort ();
  if (__builtin_classify_type (c) != 9)
    abort ();
  if (__builtin_classify_type (abort) != 5)
    abort ();
  if (__builtin_classify_type (s) != 12)
    abort ();
  if (__builtin_classify_type (u) != 13)
    abort ();
  if (__builtin_classify_type (a) != 5)
    abort ();
}

template <typename V, typename I, typename E, typename B, typename P,
	  typename R1, typename R2, typename PM, typename F,
	  typename C, typename FN, typename S, typename U, typename A>
void
bar ()
{
  E e = (E) 0;
  S s = { 0 };
  U u = { 0 };
  A a = { 0, 0 };
  B b = false;
  P p = (P) 0;
  F f = 0.0;
  C c = 0.0;
  R1 r = a[0];
  PM q = &S::s;
  static_assert (__builtin_classify_type (V) == 0, "");
  static_assert (__builtin_classify_type (I) == 1, "");
  static_assert (__builtin_classify_type (E) == 3, "");
  static_assert (__builtin_classify_type (B) == 4, "");
  static_assert (__builtin_classify_type (P) == 5, "");
  static_assert (__builtin_classify_type (R1) == 6, "");
  static_assert (__builtin_classify_type (R2) == 6, "");
  static_assert (__builtin_classify_type (PM) == 7, "");
  static_assert (__builtin_classify_type (F) == 8, "");
  static_assert (__builtin_classify_type (C) == 9, "");
  static_assert (__builtin_classify_type (FN) == 10, "");
  static_assert (__builtin_classify_type (S) == 12, "");
  static_assert (__builtin_classify_type (U) == 13, "");
  static_assert (__builtin_classify_type (A) == 14, "");
  static_assert (__builtin_classify_type (__typeof__ (a[0])) == 1, "");
  static_assert (__builtin_classify_type (__typeof__ (e)) == 3, "");
  static_assert (__builtin_classify_type (__typeof__ (b)) == 4, "");
  static_assert (__builtin_classify_type (__typeof__ (p)) == 5, "");
  static_assert (__builtin_classify_type (decltype (r)) == 6, "");
  static_assert (__builtin_classify_type (__typeof__ (q)) == 7, "");
  static_assert (__builtin_classify_type (__typeof__ (f)) == 8, "");
  static_assert (__builtin_classify_type (__typeof__ (c)) == 9, "");
  static_assert (__builtin_classify_type (__typeof__ (abort)) == 10, "");
  static_assert (__builtin_classify_type (__typeof__ (s)) == 12, "");
  static_assert (__builtin_classify_type (__typeof__ (u)) == 13, "");
  static_assert (__builtin_classify_type (__typeof__ (a)) == 14, "");
  if (__builtin_classify_type (a[0]) != 1)
    abort ();
  if (__builtin_classify_type (e) != 3)
    abort ();
  if (__builtin_classify_type (b) != 4)
    abort ();
  if (__builtin_classify_type (p) != 5)
    abort ();
  if (__builtin_classify_type (r) != 1)
    abort ();
  if (__builtin_classify_type (q) != 7)
    abort ();
  if (__builtin_classify_type (f) != 8)
    abort ();
  if (__builtin_classify_type (c) != 9)
    abort ();
  if (__builtin_classify_type (abort) != 5)
    abort ();
  if (__builtin_classify_type (s) != 12)
    abort ();
  if (__builtin_classify_type (u) != 13)
    abort ();
  if (__builtin_classify_type (a) != 5)
    abort ();
}

int
main ()
{
  enum E { E1 };
  struct S { int s; };
  union U { int u; };
  foo <0> ();
  bar <void, int, E, bool, const char *, int &, int &&, int S::*,
       float, _Complex double, int (int, int), S, U, int [2]> ();
}
