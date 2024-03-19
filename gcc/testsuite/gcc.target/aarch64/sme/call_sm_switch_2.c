// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }

void ns_callee ();
 void s_callee () [[arm::streaming]];
 void sc_callee () [[arm::streaming_compatible]];

struct callbacks {
  void (*ns_ptr) ();
   void (*s_ptr) () [[arm::streaming]];
   void (*sc_ptr) () [[arm::streaming_compatible]];
};

void
n_caller (struct callbacks *c)
{
  ns_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->sc_ptr ();
}

void
s_caller (struct callbacks *c) [[arm::streaming]]
{
  s_callee ();
  sc_callee ();

  c->s_ptr ();
  c->sc_ptr ();
}

void
sc_caller (struct callbacks *c) [[arm::streaming_compatible]]
{
  sc_callee ();

  c->sc_ptr ();
}

// { dg-final { scan-assembler-not {[dpqz][0-9]+,} } }
// { dg-final { scan-assembler-not {smstart\tsm} } }
// { dg-final { scan-assembler-not {smstop\tsm} } }
