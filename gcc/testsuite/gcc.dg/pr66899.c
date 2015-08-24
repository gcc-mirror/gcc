/* { dg-do compile } */
/* { dg-options "-Os -fprofile-arcs" } */
/* { dg-require-profiling "-fprofile-generate" } */

struct
{
  int authority;
} * a, *b, c, d;
int e, f;
static int
fn1 ()
{
  if (a)
    goto verified;
  if (b)
    goto matched;
  return -126;
matched:
  e = 0;
verified:
  if (b)
    for (; &c != b; c = d)
      ;
  return 0;
}

int
fn2 ()
{
  for (;;)
    {
      f = fn1 ();
      switch (f)
        {
        case -126:
          continue;
        default:
          return 0;
        }
    }
}

