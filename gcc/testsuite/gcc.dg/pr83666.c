/* PR debug/83666 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -Wno-psabi" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef int __attribute__ ((vector_size (64))) V;

int c, d;
short e;
V g;

V
bar (void)
{
  g[1] = d;
  do
    {
      e += c;
      g = g > 0;
    }
  while (g[1]);
  return g;
}

void
foo (void)
{
  int x = bar ()[3];
}
