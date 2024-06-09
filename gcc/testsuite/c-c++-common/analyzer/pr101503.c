/* { dg-additional-options "--param analyzer-max-svalue-depth=0 -Wno-analyzer-symbol-too-complex" } */

int val;

int
fn (void)
{
  val = fn ();

  return 0;
}
