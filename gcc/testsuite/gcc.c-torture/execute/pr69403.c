/* PR target/69403.  */

int a, b, c;

__attribute__ ((__noinline__)) int
fn1 ()
{
  if ((b | (a != (a & c))) == 1)
    __builtin_abort ();
  return 0;
}

int
main (void)
{
  a = 5;
  c = 1;
  b = 6;
  return fn1 ();
}
