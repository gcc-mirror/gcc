/* { dg-additional-options "--param analyzer-max-svalue-depth=0" } */

int val;

int
fn (void)
{
  val = fn ();

  return 0;
}
