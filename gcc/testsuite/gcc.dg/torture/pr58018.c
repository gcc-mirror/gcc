/* PR tree-optimization/58018 */
/* { dg-do compile } */

int a, b, c, d, e;

void
bar (int p)
{
  int f = b;
  e &= p <= (f ^= 0);
}
    
void
foo ()
{
  for (; d; d++)
    {
      bar (a && c);
      bar (0);
      bar (1);
    }
}
