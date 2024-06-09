/* { dg-do run { target { c || c++11 } } } */

#if !defined(__cplusplus) && __STDC_VERSION__ <= 201710L
#define static_assert _Static_assert
#define bool _Bool
#define false ((_Bool) 0)
#endif
#ifdef __cplusplus
extern "C" void abort ();
#else
extern void abort (void);
#endif

int
main ()
{
  enum E { E1 } e = E1;
  struct S { int s; } s = { 0 };
  union U { int u; } u = { 0 };
  int a[2] = { 0, 0 };
  bool b = false;
  const char *p = (const char *) 0;
  float f = 0.0;
  _Complex double c = 0.0;
  typedef int VI __attribute__((vector_size (4 * sizeof (int))));
  typedef float VF __attribute__((vector_size (4 * sizeof (int))));
  VI vi = { 0, 0, 0, 0 };
  VF vf = { 0.0f, 0.0f, 0.0f, 0.0f };
#ifdef __cplusplus
  struct T { void foo (); };
  int &r = a[0];
  int S::*q = &S::s;
#endif
  static_assert (__builtin_classify_type (void) == 0, "");
  static_assert (__builtin_classify_type (int) == 1, "");
  static_assert (__builtin_classify_type (enum E) == 3, "");
  static_assert (__builtin_classify_type (bool) == 4, "");
  static_assert (__builtin_classify_type (const char *) == 5, "");
#ifdef __cplusplus
  static_assert (__builtin_classify_type (int &) == 6, "");
  static_assert (__builtin_classify_type (int &&) == 6, "");
  static_assert (__builtin_classify_type (int S::*) == 7, "");
#endif
  static_assert (__builtin_classify_type (float) == 8, "");
  static_assert (__builtin_classify_type (_Complex double) == 9, "");
  static_assert (__builtin_classify_type (int (int, int)) == 10, "");
  static_assert (__builtin_classify_type (struct S) == 12, "");
  static_assert (__builtin_classify_type (union U) == 13, "");
  static_assert (__builtin_classify_type (int [2]) == 14, "");
  static_assert (__builtin_classify_type (VI) == 19, "");
  static_assert (__builtin_classify_type (VF) == 19, "");
  static_assert (__builtin_classify_type (__typeof__ (a[0])) == 1, "");
  static_assert (__builtin_classify_type (__typeof__ (e)) == 3, "");
  static_assert (__builtin_classify_type (__typeof__ (b)) == 4, "");
  static_assert (__builtin_classify_type (__typeof__ (p)) == 5, "");
#ifdef __cplusplus
  static_assert (__builtin_classify_type (decltype (r)) == 6, "");
  static_assert (__builtin_classify_type (__typeof__ (q)) == 7, "");
#endif
  static_assert (__builtin_classify_type (__typeof__ (f)) == 8, "");
  static_assert (__builtin_classify_type (__typeof__ (c)) == 9, "");
  static_assert (__builtin_classify_type (__typeof__ (main)) == 10, "");
  static_assert (__builtin_classify_type (__typeof__ (s)) == 12, "");
  static_assert (__builtin_classify_type (__typeof__ (u)) == 13, "");
  static_assert (__builtin_classify_type (__typeof__ (a)) == 14, "");
  static_assert (__builtin_classify_type (__typeof__ (vi)) == 19, "");
  static_assert (__builtin_classify_type (__typeof__ (vf)) == 19, "");
#ifndef __cplusplus
  static_assert (__builtin_classify_type (a[0]) == 1, "");
  static_assert (__builtin_classify_type (e) == 1, "");
  static_assert (__builtin_classify_type (b) == 1, "");
  static_assert (__builtin_classify_type (p) == 5, "");
  static_assert (__builtin_classify_type (f) == 8, "");
  static_assert (__builtin_classify_type (c) == 9, "");
  static_assert (__builtin_classify_type (main) == 5, "");
  static_assert (__builtin_classify_type (s) == 12, "");
  static_assert (__builtin_classify_type (u) == 13, "");
  static_assert (__builtin_classify_type (a) == 5, "");
#endif
  if (__builtin_classify_type (a[0]) != 1)
    abort ();
#ifdef __cplusplus
  if (__builtin_classify_type (e) != 3)
    abort ();
  if (__builtin_classify_type (b) != 4)
    abort ();
#else
  if (__builtin_classify_type (e) != 1)
    abort ();
  if (__builtin_classify_type (b) != 1)
    abort ();
#endif
  if (__builtin_classify_type (p) != 5)
    abort ();
#ifdef __cplusplus
  if (__builtin_classify_type (r) != 1)
    abort ();
  if (__builtin_classify_type (q) != 7)
    abort ();
#endif
  if (__builtin_classify_type (f) != 8)
    abort ();
  if (__builtin_classify_type (c) != 9)
    abort ();
  if (__builtin_classify_type (main) != 5)
    abort ();
  if (__builtin_classify_type (s) != 12)
    abort ();
  if (__builtin_classify_type (u) != 13)
    abort ();
  if (__builtin_classify_type (a) != 5)
    abort ();
  if (__builtin_classify_type (vi) != 19)
    abort ();
  if (__builtin_classify_type (vf) != 19)
    abort ();
}
