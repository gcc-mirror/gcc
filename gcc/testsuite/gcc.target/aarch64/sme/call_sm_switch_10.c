// { dg-options "" }

#pragma GCC target "+nosme"

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
  s_callee (); // { dg-error "calling a streaming function requires the ISA extension 'sme'" }
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr (); // { dg-error "calling a streaming function requires the ISA extension 'sme'" }
  c->sc_ptr ();
}

void
sc_caller_sme (struct callbacks *c) [[arm::streaming_compatible]]
{
  ns_callee ();
  s_callee (); // { dg-error "calling a streaming function requires the ISA extension 'sme'" }
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr (); // { dg-error "calling a streaming function requires the ISA extension 'sme'" }
  c->sc_ptr ();
}
