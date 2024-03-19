/* { dg-options "-O2" } */

void shared_callee () [[arm::inout("za")]];
[[arm::new("za")]] __attribute__((noipa)) void new_callee () {}
void normal_callee ();

void
normal_to_new ()
{
  new_callee ();
}
/* { dg-final { scan-assembler {\tb\tnew_callee} } } */

void
normal_to_normal ()
{
  normal_callee ();
}
/* { dg-final { scan-assembler {\tb\tnormal_callee} } } */
