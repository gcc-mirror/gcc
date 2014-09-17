/* PR debug/63284 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

int a[10], *b, *d, c, f;
int fn2 (void);
void fn3 (void);
void fn4 (int);

static int
fn1 (int x)
{
  int e = a[0];
  if (e)
    return 1;
  if (b)
    switch (x)
      {
      case 1:
        if (d)
          e = fn2 ();
        else
          fn3 ();
        break;
      case 0:
        if (d)
          {
            fn3 ();
            if (c)
              fn4 (1);
          }
        else
          fn4 (0);
      }
  return e;
}

void
fn6 (void)
{
  f = fn1 (0);
}
