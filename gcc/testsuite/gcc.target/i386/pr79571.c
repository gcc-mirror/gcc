/* PR rtl-optimization/79571 */
/* { dg-do compile } */
/* { dg-options "-O -mno-sse -w" } */

struct a
{
  int b;
  int *c
} h;
struct d
{
  struct a e
};
struct fd
{
  struct d *d
} i;
g;
j ()
{
  unsigned a = g;
  i = (struct fd){a & 3};
  struct fd f = i;
  h = f.d->e;
}
