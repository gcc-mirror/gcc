/* PR tree-optimization/98084 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct
{
  unsigned a : 10;
} b;

int c;
void e();
void d ()
{
  c = b.a;
  if (c == 8 || c == 0)
    ;
  else if (c > 8 * 8)
    ;
  else if (c < 8 * 8)
    e ();
}
