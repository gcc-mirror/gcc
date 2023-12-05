/* { dg-options "-O2" } */

void sc_callee () [[arm::streaming_compatible]];
void s_callee () [[arm::streaming]];
void n_callee ();

[[arm::locally_streaming]] __attribute__((noipa)) void
sc_ls_callee () [[arm::streaming_compatible]] {}
[[arm::locally_streaming]] __attribute__((noipa)) void
n_ls_callee () {}

[[arm::locally_streaming]] void
sc_to_sc () [[arm::streaming_compatible]]
{
  sc_callee ();
}
/* { dg-final { scan-assembler {\tb\tsc_callee} } } */

[[arm::locally_streaming]] void
sc_to_s () [[arm::streaming_compatible]]
{
  s_callee ();
}
/* { dg-final { scan-assembler {\tbl\ts_callee} } } */

[[arm::locally_streaming]] void
sc_to_n () [[arm::streaming_compatible]]
{
  n_callee ();
}
/* { dg-final { scan-assembler {\tbl\tn_callee} } } */

[[arm::locally_streaming]] void
sc_to_sc_ls () [[arm::streaming_compatible]]
{
  sc_ls_callee ();
}
/* { dg-final { scan-assembler {\tb\tsc_ls_callee} } } */

[[arm::locally_streaming]] void
sc_to_n_ls () [[arm::streaming_compatible]]
{
  n_ls_callee ();
}
/* { dg-final { scan-assembler {\tbl\tn_ls_callee} } } */
