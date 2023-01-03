/* PR middle-end/108264 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpic" { target fpic } } */

int v;
extern int bar (void);

static inline void
foo (char *d)
{
  switch (bar ())
    {
    case 2:
      d[0] = d[1] = d[2] = d[3] = v;
      break;
    case 4:
      d[0] = 0;
    }
}

int
baz (int x)
{
  foo ((char *) &x);
  return x;
}
