/* PR tree-optimization/67953 */
/* { dg-do run } */

unsigned int
fn1 (signed int a)
{
  return (unsigned int) a - ((a / 3) * 3);
}

unsigned int
fn2 (signed int a)
{
  return a - ((a / 3) * 3);
}

unsigned int
fn3 (int a)
{
  return a - (unsigned) ((a / 3) * 3);
}

signed int
fn4 (int a)
{
  return (unsigned) a - (unsigned) ((a / 3) * 3);
}

int
main ()
{
  if (fn1 (-5) != -2
      || fn2 (-5) != -2
      || fn3 (-5) != -2
      || fn4 (-5) != -2)
    __builtin_abort ();
}
