// Bug c++/83503 - bogus -Wattributes for const and pure on function template
// specialization
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

int global;

template <class T>
int __attribute__ ((pure))
f (T);

template <>
int __attribute__ ((const)) f<int> (int);   // { dg-bogus "ignoring attribute .const." }

void f_pure_primary_elim ();
void f_pure_primary_keep ();
void f_const_spec_elim ();

void call_pure_primary_elim (double x)
{
  // Only the first call to f(x) must be emitted, the second one
  // is expected to be eliminated because the primary template
  // is pure.
  int i0 = f (x);
  int i1 = f (x);
  if (i0 != i1)
    f_pure_primary_elim ();
}

void call_pure_primary_keep (const char *s)
{
  // Both calls to f(x) must be emitted because the primary is
  // pure and may read global.
  int i0 = f (s);
  global = 123;
  int i1 = f (s);
  if (i0 != i1)
    f_pure_primary_keep ();
}

void call_const_spec_elim (int i)
{
  // Only the first call to f(x) must be emitted, the second
  // one is expected to be eliminated again, this time because
  // unlike the pure primary, the specialization is const.
  int i0 = f (i);
  global = 123;
  int i1 = f (i);
  if (i0 != i1)
    f_const_spec_elim ();
}

template <class T>
int __attribute__ ((const))
g (T);

template <>
int __attribute__ ((pure)) g<int> (int);   // { dg-bogus "ignoring attribute .const." }

template <class T>
int __attribute__ ((const))
h (T);

template <class T>
int __attribute__ ((pure))
h (const T*);

template <>
int h<int> (int);

template <>
int h<int*> (int*);

extern void h_const_primary_elim ();
extern void h_pure_cstptr_elim ();
extern void h_cstptr_keep ();
extern void h_int_keep ();
extern void h_intptr_keep ();

void call_const_primary_elim (double x)
{
  // Only the first call to h(x) must be emitted, the second one
  // is expected to be eliminated.
  int i0 = h (x);
  int i1 = h (x);

  if (i0 != i1)                   // must be folded into false
    h_const_primary_elim ();        // must be eliminated
}

void call_pure_cstptr_elim (const void *p)
{
  // Only the first call to h(x) must be emitted, the second one
  // is expected to be eliminated.  This verifies that h<const
  // void*>*() is treated as const in this context.
  int i0 = h (p);
  int i1 = h (p);

  if (i0 != i1)                   // must be folded into false
    h_pure_cstptr_elim ();          // must be eliminated
}

void call_cstptr_keep (const void *p)
{
  // Because of the store to the global, both calls to h(p) must
  // be emitted.  This verifies that h<const void*>*() is not
  // treated as const.
  int i0 = h (p);
  global = 123;
  int i1 = h (p);

  if (i0 != i1)                   // must not be folded
    h_cstptr_keep ();             // must be emitted
}

void call_int_keep (int i)
{
  // Both calls to h(i) must be emitted.
  int i0 = h (i);
  int i1 = h (i);

  if (i0 != i1)                   // must not be folded
    h_int_keep ();                // must be emitted
}

void call_intptr_keep (int *ip)
{
  // Both calls to h(ip) must be emitted.
  int i0 = h (ip);
  int i1 = h (ip);

  if (i0 != i1)                   // must not be folded
    h_intptr_keep ();             // must be emitted
}

// { dg-final { scan-tree-dump-not "f_pure_primary_elim" "optimized" } }
// { dg-final { scan-tree-dump-not "f_const_primary_elim" "optimized" } }
// { dg-final { scan-tree-dump-not "f_const_spec_elim" "optimized" } }
// { dg-final { scan-tree-dump-not "h_pure_cstptr_elim" "optimized" } }

// { dg-final { scan-tree-dump-times "f_pure_primary_keep" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "h_cstptr_keep" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "h_int_keep" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "h_intptr_keep" 1 "optimized" } }
