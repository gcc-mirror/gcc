// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }

#include <arm_sve.h>

svbool_t ns_callee ();
 svbool_t s_callee () [[arm::streaming]];
 svbool_t sc_callee () [[arm::streaming_compatible]];

struct callbacks {
  svbool_t (*ns_ptr) ();
   svbool_t (*s_ptr) () [[arm::streaming]];
   svbool_t (*sc_ptr) () [[arm::streaming_compatible]];
};

svbool_t
n_caller (struct callbacks *c)
{
  ns_callee ();
  sc_callee ();

  c->ns_ptr ();
  return c->sc_ptr ();
}

svbool_t
s_caller (struct callbacks *c) [[arm::streaming]]
{
  s_callee ();
  sc_callee ();

  c->s_ptr ();
  return c->sc_ptr ();
}

svbool_t
sc_caller (struct callbacks *c) [[arm::streaming_compatible]]
{
  sc_callee ();

  return c->sc_ptr ();
}

// { dg-final { scan-assembler-not {[dpqz][0-9]+,} } }
// { dg-final { scan-assembler-not {smstart\tsm} } }
// { dg-final { scan-assembler-not {smstop\tsm} } }
