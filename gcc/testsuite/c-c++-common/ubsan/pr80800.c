/* PR sanitizer/80800 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int n = 20000;
int z = 0;

int
fn1 (void)
{
  return (n * 10000 * z) * 50;
}

int
fn2 (void)
{
  return (10000 * n * z) * 50;
}

int
main ()
{
  fn1 ();
  fn2 ();
}
