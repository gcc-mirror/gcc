/* { dg-do compile } */

int a, b, c, d;

int
fn1 ()
{
  for (; c; c++)
    for (b = 0; b < 2; b++)
      d = a - d;
  return d; 
}

