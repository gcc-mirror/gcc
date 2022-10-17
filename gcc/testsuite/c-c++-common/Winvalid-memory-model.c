/* PR middle-end/99612 - Missing warning on incorrect memory order without
   -Wsystem-headers
   Verify that constants are propagated through calls to inline functions
   even at -O0.
   Also verify that the informational notes after each warning mention
   the valid memore models for each function.
   { dg-do compile }
   { dg-options "-O0 -ftrack-macro-expansion=0" } */

#if !__cplusplus
# define bool _Bool
#endif

extern int ei;

static __attribute__ ((always_inline)) inline
int retval (int val)
{
  return val;
}

void test_load (int *pi)
{
  int relaxed = retval (__ATOMIC_RELAXED);
  *pi++ = __atomic_load_n (&ei, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  *pi++ = __atomic_load_n (&ei, consume);

  int acquire = retval (__ATOMIC_ACQUIRE);
  *pi++ = __atomic_load_n (&ei, acquire);

  int release = retval (__ATOMIC_RELEASE);
  *pi++ = __atomic_load_n (&ei, release);   // { dg-warning "invalid memory model 'memory_order_release'" }
  // { dg-message "valid models are 'memory_order_relaxed', 'memory_order_seq_cst', 'memory_order_acquire', 'memory_order_consume'" "note" { target *-*-* } .-1 }

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  *pi++ = __atomic_load_n (&ei, acq_rel);   // { dg-warning "invalid memory model 'memory_order_acq_rel'" }

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  *pi++ = __atomic_load_n (&ei, seq_cst);

  /* Verify a nonconstant range.  */
  int r0_1 = *pi++;
  if (r0_1 < 0 || 1 < r0_1)
    r0_1 = 0;
  *pi++ = __atomic_load_n (&ei, r0_1);

  /* Verify an unbounded range.  */
  int unknown = *pi++;
  *pi++ = __atomic_load_n (&ei, unknown);
}


void test_store (int *pi, int x)
{
  int relaxed = retval (__ATOMIC_RELAXED);
  __atomic_store_n (pi++, x, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  __atomic_store_n (pi++, x, consume);      // { dg-warning "invalid memory model 'memory_order_consume'" }
  // { dg-message "valid models are 'memory_order_relaxed', 'memory_order_seq_cst', 'memory_order_release'" "note" { target *-*-* } .-1 }

  int acquire = retval (__ATOMIC_ACQUIRE);
  __atomic_store_n (pi++, x, acquire);      // { dg-warning "invalid memory model 'memory_order_acquire'" }

  int release = retval (__ATOMIC_RELEASE);
  __atomic_store_n (pi++, x, release);

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  __atomic_store_n (pi++, x, acq_rel);      // { dg-warning "invalid memory model 'memory_order_acq_rel'" }

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  __atomic_store_n (pi++, x, seq_cst);

  int unknown = *pi++;
  __atomic_store_n (pi++, x, unknown);
}


/* All memory models are valid.  */

void test_exchange (int *pi, int x)
{
  int relaxed = retval (__ATOMIC_RELAXED);
  __atomic_exchange_n (pi++, x, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  __atomic_exchange_n (pi++, x, consume);

  int acquire = retval (__ATOMIC_ACQUIRE);
  __atomic_exchange_n (pi++, x, acquire);

  int release = retval (__ATOMIC_RELEASE);
  __atomic_exchange_n (pi++, x, release);

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  __atomic_exchange_n (pi++, x, acq_rel);

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  __atomic_exchange_n (pi++, x, seq_cst);

  int unknown = *pi++;
  __atomic_exchange_n (pi++, x, unknown);
}


void test_compare_exchange (int *pi, int *pj, bool weak)
{
#define cmpxchg(x, expect, desire, sucs_ord, fail_ord) \
  __atomic_compare_exchange_n (x, expect, desire, weak, sucs_ord, fail_ord)

  int relaxed = retval (__ATOMIC_RELAXED);
  cmpxchg (&ei, pi++, *pj++, relaxed, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  cmpxchg (&ei, pi++, *pj++, relaxed, consume);   // { dg-warning "failure memory model 'memory_order_consume' cannot be stronger than success memory model 'memory_order_relaxed'" }

  int acquire = retval (__ATOMIC_ACQUIRE);
  cmpxchg (&ei, pi++, *pj++, relaxed, acquire);   // { dg-warning "failure memory model 'memory_order_acquire' cannot be stronger than success memory model 'memory_order_relaxed'" }

  int release = retval (__ATOMIC_RELEASE);
  cmpxchg (&ei, pi++, *pj++, relaxed, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  cmpxchg (&ei, pi++, *pj++, relaxed, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  cmpxchg (&ei, pi++, *pj++, relaxed, seq_cst);   // { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger than success memory model 'memory_order_relaxed'" }


  cmpxchg (&ei, pi++, *pj++, consume, relaxed);
  cmpxchg (&ei, pi++, *pj++, consume, consume);
  cmpxchg (&ei, pi++, *pj++, consume, acquire);   // { dg-warning "failure memory model 'memory_order_acquire' cannot be stronger than success memory model 'memory_order_consume'" }
  cmpxchg (&ei, pi++, *pj++, consume, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }
  cmpxchg (&ei, pi++, *pj++, consume, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }
  cmpxchg (&ei, pi++, *pj++, consume, seq_cst);   // { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger than success memory model 'memory_order_consume'" }

  cmpxchg (&ei, pi++, *pj++, acquire, relaxed);
  cmpxchg (&ei, pi++, *pj++, acquire, consume);
  cmpxchg (&ei, pi++, *pj++, acquire, acquire);
  cmpxchg (&ei, pi++, *pj++, acquire, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }
  cmpxchg (&ei, pi++, *pj++, acquire, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }
  cmpxchg (&ei, pi++, *pj++, acquire, seq_cst);   // { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger than success memory model 'memory_order_acquire'" }

  cmpxchg (&ei, pi++, *pj++, release, relaxed);
  cmpxchg (&ei, pi++, *pj++, release, consume);
  cmpxchg (&ei, pi++, *pj++, release, acquire);
  cmpxchg (&ei, pi++, *pj++, release, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }
  cmpxchg (&ei, pi++, *pj++, release, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }
  cmpxchg (&ei, pi++, *pj++, release, seq_cst);   // { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger than success memory model 'memory_order_release'" }

  cmpxchg (&ei, pi++, *pj++, acq_rel, relaxed);
  cmpxchg (&ei, pi++, *pj++, acq_rel, consume);
  cmpxchg (&ei, pi++, *pj++, acq_rel, acquire);
  cmpxchg (&ei, pi++, *pj++, acq_rel, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }
  cmpxchg (&ei, pi++, *pj++, acq_rel, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }
  cmpxchg (&ei, pi++, *pj++, acq_rel, seq_cst);   // { dg-warning "failure memory model 'memory_order_seq_cst' cannot be stronger than success memory model 'memory_order_acq_rel'" }

  cmpxchg (&ei, pi++, *pj++, seq_cst, relaxed);
  cmpxchg (&ei, pi++, *pj++, seq_cst, consume);
  cmpxchg (&ei, pi++, *pj++, seq_cst, acquire);
  cmpxchg (&ei, pi++, *pj++, seq_cst, release);   // { dg-warning "invalid failure memory model 'memory_order_release'" }
  cmpxchg (&ei, pi++, *pj++, seq_cst, acq_rel);   // { dg-warning "invalid failure memory model 'memory_order_acq_rel'" }
  cmpxchg (&ei, pi++, *pj++, seq_cst, seq_cst);

  int unknown = *pi++;
  cmpxchg (&ei, pi++, *pj++, unknown, seq_cst);
  cmpxchg (&ei, pi++, *pj++, relaxed, unknown);
}


/* All memory models are valid.  */

void test_add_fetch (unsigned *pi, unsigned x)
{
  int relaxed = retval (__ATOMIC_RELAXED);
  __atomic_add_fetch (pi++, x, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  __atomic_add_fetch (pi++, x, consume);

  int acquire = retval (__ATOMIC_ACQUIRE);
  __atomic_add_fetch (pi++, x, acquire);

  int release = retval (__ATOMIC_RELEASE);
  __atomic_add_fetch (pi++, x, release);

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  __atomic_add_fetch (pi++, x, acq_rel);

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  __atomic_add_fetch (pi++, x, seq_cst);

  int invalid;
  if (x & 1)
    {
      invalid = retval (123);
      __atomic_add_fetch (pi++, x, invalid);  // { dg-warning "invalid memory model 123 for '\(unsigned int \)?__atomic_add_fetch" }
    }
  else
    {
      invalid = retval (456);
      __atomic_add_fetch (pi++, x, invalid);  // { dg-warning "invalid memory model 456 for '\(unsigned int \)?__atomic_add_fetch" }
    }
}

void test_sub_fetch (unsigned *pi, unsigned x)
{
  int relaxed = retval (__ATOMIC_RELAXED);
  __atomic_sub_fetch (pi++, x, relaxed);

  int consume = retval (__ATOMIC_CONSUME);
  __atomic_sub_fetch (pi++, x, consume);

  int acquire = retval (__ATOMIC_ACQUIRE);
  __atomic_sub_fetch (pi++, x, acquire);

  int release = retval (__ATOMIC_RELEASE);
  __atomic_sub_fetch (pi++, x, release);

  int acq_rel = retval (__ATOMIC_ACQ_REL);
  __atomic_sub_fetch (pi++, x, acq_rel);

  int seq_cst = retval (__ATOMIC_SEQ_CST);
  __atomic_sub_fetch (pi++, x, seq_cst);

  int invalid;
  if (x & 1)
    {
      invalid = retval (123);
      __atomic_sub_fetch (pi++, x, invalid);  // { dg-warning "invalid memory model 123 for '\(unsigned int \)?__atomic_sub_fetch" }
    }
  else
    {
      invalid = retval (456);
      __atomic_sub_fetch (pi++, x, invalid);  // { dg-warning "invalid memory model 456 for '\(unsigned int \)?__atomic_sub_fetch" }
    }
}
