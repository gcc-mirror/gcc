/* PR target/92225 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2 -mno-sse4" } */

void a (long);

unsigned *b;

void
c ()
{
  long d = 2;
  int e = 0;
  
  for (; e < 1024; e++)
    if (b[e] > d)
      d = b[e];
  a (d);
}
