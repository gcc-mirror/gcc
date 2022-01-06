/* PR middle-end/99612 - Missing warning on incorrect memory order without
   -Wsystem-headers
   Verify warings for basic atomic functions with no optimization.
   { dg-do compile { target c++11 } }
   { dg-options "-O0 -Wall" } */

#include <atomic>

static const std::memory_order relaxed = std::memory_order_relaxed;
static const std::memory_order consume = std::memory_order_consume;
static const std::memory_order acquire = std::memory_order_acquire;
static const std::memory_order release = std::memory_order_release;
static const std::memory_order acq_rel = std::memory_order_acq_rel;
static const std::memory_order seq_cst = std::memory_order_seq_cst;

extern std::atomic<int> eai;

void test_load (int *pi)
{
  *pi++ = eai.load (relaxed);
  *pi++ = eai.load (consume);
  *pi++ = eai.load (acquire);
  *pi++ = eai.load (release);       // warning
  *pi++ = eai.load (acq_rel);       // warning
  *pi++ = eai.load (seq_cst);
}

/* { dg-regexp " *inlined from \[^\n\r\]+.C:23:.*" "" { target *-*-* } 0 }
   { dg-regexp " *inlined from \[^\n\r\]+.C:24:.*" "" { target *-*-* } 0 }
   { dg-warning "__atomic_load\[^\n\r\]* \\\[-Winvalid-memory-model" "warning" { target *-*-* } 0 } */


void test_store (int *pi)
{
  eai.store (*pi++, relaxed);
  eai.store (*pi++, consume);       // warning
  eai.store (*pi++, acquire);       // warning
  eai.store (*pi++, release);
  eai.store (*pi++, acq_rel);       // warning
  eai.store (*pi++, seq_cst);
}

/* { dg-regexp " *inlined from \[^\n\r\]+.C:36:.*" "" { target *-*-* } 0 }
   { dg-regexp " *inlined from \[^\n\r\]+.C:37:.*" "" { target *-*-* } 0 }
   { dg-regexp " *inlined from \[^\n\r\]+.C:39:.*" "" { target *-*-* } 0 }
   { dg-warning "__atomic_store\[^\n\r]* \\\[-Winvalid-memory-model" "warning" { target *-*-* } 0 } */


void test_exchange (const int *pi)
{
  eai.exchange (*pi++, relaxed);
  eai.exchange (*pi++, consume);
  eai.exchange (*pi++, acquire);
  eai.exchange (*pi++, release);
  eai.exchange (*pi++, acq_rel);
  eai.exchange (*pi++, seq_cst);
}

/* The following tests fail because std::atomic_compare_exchange_weak_explicit
   is not declared with attribute always_inline (like the member functions
   above are).  */

void test_compare_exchange (int *pi, int *pj)
{
#define cmpxchg(x, y, z, o1, o2) \
  std::atomic_compare_exchange_weak_explicit (x, y, z, o1, o2)

  cmpxchg (&eai, pi++, *pj++, relaxed, relaxed);
  cmpxchg (&eai, pi++, *pj++, relaxed, consume);  // warning
  cmpxchg (&eai, pi++, *pj++, relaxed, acquire);  // warning
  cmpxchg (&eai, pi++, *pj++, relaxed, release);  // warning
  cmpxchg (&eai, pi++, *pj++, relaxed, acq_rel);  // warning
  cmpxchg (&eai, pi++, *pj++, relaxed, seq_cst);  // warning
  cmpxchg (&eai, pi++, *pj++, relaxed, relaxed);

  /* HACK: xfail doesn't seem to work for the dg-regexp directives below,
     so disable them by prepending an X to their names...
    { Xdg-regexp " *inlined from \[^\n\r\]+.C:66:.*" "" { xfail *-*-* } 0 }
    { Xdg-regexp " *inlined from \[^\n\r\]+.C:67:.*" "" { xfail *-*-* } 0 }
    { Xdg-regexp " *inlined from \[^\n\r\]+.C:68:.*" "" { xfail *-*-* } 0 }
    { Xdg-regexp " *inlined from \[^\n\r\]+.C:69:.*" "" { xfail *-*-* } 0 }
    { Xdg-regexp " *inlined from \[^\n\r\]+.C:70:.*" "" { xfail *-*-* } 0 }
    { dg-warning "__atomic_compare_exchange\[^\n\r\]* \\\[-Winvalid-memory-model" "cmpxchg 1" { xfail *-*-* } 0 } */
}
