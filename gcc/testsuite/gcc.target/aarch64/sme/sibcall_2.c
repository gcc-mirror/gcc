/* { dg-options "-O2" } */

void sc_callee () [[arm::streaming_compatible]];
void s_callee () [[arm::streaming]];
void n_callee ();

[[arm::locally_streaming]] __attribute__((noipa)) void
sc_ls_callee () [[arm::streaming_compatible]] {}
[[arm::locally_streaming]] __attribute__((noipa)) void
n_ls_callee () {}

void
s_to_sc () [[arm::streaming]]
{
  sc_callee ();
}
/* { dg-final { scan-assembler {\tb\tsc_callee} } } */

void
s_to_s () [[arm::streaming]]
{
  s_callee ();
}
/* { dg-final { scan-assembler {\tb\ts_callee} } } */

void
s_to_n () [[arm::streaming]]
{
  n_callee ();
}
/* { dg-final { scan-assembler {\tbl\tn_callee} } } */

void
s_to_sc_ls () [[arm::streaming]]
{
  sc_ls_callee ();
}
/* { dg-final { scan-assembler {\tb\tsc_ls_callee} } } */

void
s_to_n_ls () [[arm::streaming]]
{
  n_ls_callee ();
}
/* { dg-final { scan-assembler {\tbl\tn_ls_callee} } } */
