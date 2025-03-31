/* { dg-do compile } */
/* { dg-options "-msse4" } */

typedef long long v2di __attribute__((vector_size(16)));

static inline __attribute__((always_inline))
int rte_trace_feature_is_enabled() { return 1; } /* { dg-error "inlining failed" } */

void __attribute__((target ("no-sse3"))) __attribute__((target ("no-sse4")))
rte_eal_trace_generic_void_init(void)
{
  if (!rte_trace_feature_is_enabled()) return;
  __asm__ volatile ("" : : : "memory");
}

