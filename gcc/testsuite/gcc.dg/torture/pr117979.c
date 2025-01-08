/* { dg-do compile } */

int a, b;
void foo (void);
int __attribute__((returns_twice)) bar (int);

int __attribute__((const))
baz (int f)
{
  if (f)
    {
      l:;
      for (f = 0; f < 6; ++f)
        if (bar (b))
          goto l;
      for (;; a--)
        ;
    }
  foo ();
  return 0;
}
