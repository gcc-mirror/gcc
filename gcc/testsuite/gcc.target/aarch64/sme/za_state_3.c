// { dg-options "" }

void normal_callee ();
void in_callee () [[arm::in("za")]];
void out_callee () [[arm::out("za")]];
void inout_callee () [[arm::inout("za")]];
void preserves_callee () [[arm::preserves("za")]];

struct callbacks {
  void (*normal_ptr) ();
  void (*in_ptr) () [[arm::in("za")]];
  void (*out_ptr) () [[arm::out("za")]];
  void (*inout_ptr) () [[arm::inout("za")]];
  void (*preserves_ptr) () [[arm::preserves("za")]];
};

void
normal_caller (struct callbacks *c)
{
  normal_callee ();
  in_callee (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  out_callee (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  inout_callee (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  preserves_callee (); // { dg-error {call to a function that shares SME state from a function that has no SME state} }

  c->normal_ptr ();
  c->in_ptr (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  c->out_ptr (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  c->inout_ptr (); // { dg-error {call to a function that shares 'za' state from a function that has no 'za' state} }
  c->preserves_ptr (); // { dg-error {call to a function that shares SME state from a function that has no SME state} }
}
