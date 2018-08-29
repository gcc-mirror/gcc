/* PR bootstrap/83396 */
/* { dg-do compile } */
/* { dg-options "-g" } */

int fn1 (void);
void fn2 (void *, const char *);
void fn3 (void);

void
fn4 (long long x)
{
  fn3 ();
}

void
fn5 (long long x)
{
  if (x)
    fn3();
}

void
fn6 (long long x)
{
  switch (fn1 ())
    {
    case 0:
      fn5 (x);
    case 2:
      fn2 (0, "");
      break;
    case 1:
    case 3:
      fn4(x);
    case 5:
      fn2 (0, "");
    }
}
