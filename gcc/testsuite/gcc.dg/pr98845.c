/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce -fno-tree-dse" } */

int n;

__attribute__ ((returns_twice)) void
foo (void);

void
bar (void);

void
quux (int x)
{
  if (x)
    ++x;
  else
    {
      if (n)
        {
          x = 1;
          foo ();
        }
      else
        bar ();

      if (n)
        {
          ++x;
          ++n;
        }
    }
}
