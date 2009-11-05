/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "move.l \%d0,\%a0" { target *-*-*linux* } } } */

struct pts {
  int c;
};

unsigned int bar (struct pts *a, int b);

struct pts * foo (struct pts *a, int b)
{
  return (struct pts *) bar (a, b);
}
