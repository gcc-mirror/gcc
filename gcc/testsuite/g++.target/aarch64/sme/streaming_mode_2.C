// { dg-options "" }

void sc_fn () [[arm::streaming_compatible]];
void s_fn () [[arm::streaming]];
void ns_fn ();

void (*sc_fn_ptr) () [[arm::streaming_compatible]];
void (*s_fn_ptr) () [[arm::streaming]];
void (*ns_fn_ptr) ();

void
f ()
{
  sc_fn_ptr = sc_fn;
  sc_fn_ptr = s_fn; // { dg-error "invalid conversion" }
  sc_fn_ptr = ns_fn; // { dg-error "invalid conversion" }

  s_fn_ptr = sc_fn; // { dg-error "invalid conversion" }
  s_fn_ptr = s_fn;
  s_fn_ptr = ns_fn; // { dg-error "invalid conversion" }

  ns_fn_ptr = sc_fn; // { dg-error "invalid conversion" }
  ns_fn_ptr = s_fn; // { dg-error "invalid conversion" }
  ns_fn_ptr = ns_fn;
}
