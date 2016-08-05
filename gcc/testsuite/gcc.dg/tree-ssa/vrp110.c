/* { dg-options "-O2" }  */

extern void foo (void);
extern void bar (void);

void
test (int i)
{
  if (i == 1)
    return;

  typedef int t;
  t j = i;
  switch (j)
    {
    case 1:
    case 2:
      foo ();
      break;
    case 7:
      bar ();
      break;
    }
}
