/* PR tree-optimization/126641 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double x;
extern int n;

int
foo ()
{
  return n - x && x * 0;
}
