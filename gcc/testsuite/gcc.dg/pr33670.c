/* PR middle-end/33670 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsched-stalled-insns=0" } */

struct B
{
  int p;
  int n;
};
extern struct B ***b;
extern int a;

int
foo (int d, int e)
{
  int c;
  for (c = d; c <= e; c++)
    b[a][c]->n = b[a][c]->p;
}
