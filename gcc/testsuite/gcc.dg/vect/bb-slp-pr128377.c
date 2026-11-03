/* { dg-do compile } */

extern int rt (void) __attribute__ ((returns_twice, leaf));
extern void bar (void);

void
f (int *out, int x, int y, int c)
{
  bar ();
  switch (c)
    {
    case 1:
      rt ();
      out[0] = x + 1;
      out[1] = y + 2;
      out[2] = x + 3;
      out[3] = y + 4;
      break;
    case 2:
      bar ();
      break;
    }
}
