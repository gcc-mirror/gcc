// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }

__attribute__((aarch64_vector_pcs)) void ns_callee ();
__attribute__((aarch64_vector_pcs)) void s_callee () [[arm::streaming]];
__attribute__((aarch64_vector_pcs)) void sc_callee () [[arm::streaming_compatible]];

struct callbacks {
  __attribute__((aarch64_vector_pcs)) void (*ns_ptr) ();
  __attribute__((aarch64_vector_pcs)) void (*s_ptr) () [[arm::streaming]];
  __attribute__((aarch64_vector_pcs)) void (*sc_ptr) () [[arm::streaming_compatible]];
};

void __attribute__((aarch64_vector_pcs))
n_caller (struct callbacks *c)
{
  ns_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->sc_ptr ();
}

void __attribute__((aarch64_vector_pcs))
s_caller (struct callbacks *c) [[arm::streaming]]
{
  s_callee ();
  sc_callee ();

  c->s_ptr ();
  c->sc_ptr ();
}

void __attribute__((aarch64_vector_pcs))
sc_caller (struct callbacks *c) [[arm::streaming_compatible]]
{
  sc_callee ();

  c->sc_ptr ();
}

// { dg-final { scan-assembler-not {[dpqz][0-9]+,} } }
// { dg-final { scan-assembler-not {smstart\tsm} } }
// { dg-final { scan-assembler-not {smstop\tsm} } }
