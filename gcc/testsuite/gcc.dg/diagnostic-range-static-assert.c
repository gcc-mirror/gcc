/* { dg-options "-fdiagnostics-show-caret" } */

void test_nonconst_static_assert (int param)
{
  int local = 0;

  _Static_assert (param > 0, "message"); /* { dg-error "expression in static assertion is not constant" } */
/* { dg-begin-multiline-output "" }
   _Static_assert (param > 0, "message");
                   ~~~~~~^~~
{ dg-end-multiline-output "" } */

  _Static_assert (param, "message"); /* { dg-error "expression in static assertion is not constant" } */
/* { dg-begin-multiline-output "" }
   _Static_assert (param, "message");
                   ^~~~~
{ dg-end-multiline-output "" } */

  _Static_assert (local, "message"); /* { dg-error "expression in static assertion is not constant" } */
/* { dg-begin-multiline-output "" }
   _Static_assert (local, "message");
                   ^~~~~
{ dg-end-multiline-output "" } */
}
