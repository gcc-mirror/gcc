/* { dg-options "-O2" } */

void shared_callee () [[arm::inout("za")]];
[[arm::new("za")]] __attribute__((noipa)) void new_callee () {}
void normal_callee ();

void
shared_to_shared () [[arm::inout("za")]]
{
  shared_callee ();
}
/* { dg-final { scan-assembler {\tb\tshared_callee} } } */

void
shared_to_new () [[arm::inout("za")]]
{
  new_callee ();
}
/* { dg-final { scan-assembler {\tbl\tnew_callee} } } */

void
shared_to_normal () [[arm::inout("za")]]
{
  normal_callee ();
}
/* { dg-final { scan-assembler {\tbl\tnormal_callee} } } */
