/* { dg-do compile } */
/* { dg-additional-options "-w" } */

int a, *b, e;
static int **c = &b;

struct
{
  int f0;
} d;

int *
fn1 ()
{
  int f, **g = &b;
  e = a;
  for (; a;)
    for (; d.f0; d.f0++)
      ;
  *g = &f;
  return *c;
}
