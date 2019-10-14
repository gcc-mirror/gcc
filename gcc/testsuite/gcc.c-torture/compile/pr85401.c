/* { dg-do compile } */
/* { dg-options "-O2" } */

int h (void);
int i (int);

struct a b;
struct a
{
  unsigned c:4;
} d ()
{
  int e, f = b.c << 2, g = h ();
  for (; g;)
    ;
  if (e == 0)
    if (f)
      i (f);
  return b;
}
