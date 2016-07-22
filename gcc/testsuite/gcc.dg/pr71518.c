/* PR tree-optimization/71518 */
/* { dg-options "-O3" } */

int a, *b[9], c, d, e; 

static int
fn1 ()
{
  for (c = 6; c >= 0; c--)
    for (d = 0; d < 2; d++)
      {
        b[d * 2 + c] = 0;
        e = a > 1 ? : 0;
        if (e == 2) 
          return 0;
      }
  return 0;
}

int
main ()
{
  fn1 ();
  return 0; 
}
