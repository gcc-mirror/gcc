/* { dg-do compile } */
/* { dg-options "-O1" } */

int m;

int
undefined (int);

__attribute__ ((pure, returns_twice)) int
bar (void)
{
  m = 1;
  while (m)
    m = 2;

  return m;
}

int
foo (void)
{
  return undefined (bar ());
}
