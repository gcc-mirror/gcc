/* { dg-do link } */
/* { dg-options "-mminimal-toc" { target { powerpc64-*-* && lp64 } } } */

char *strchr (const char *, int);

int
foo (int a)
{
  int b;

  b = 0;
  if ("/"[1] != '\0')
    if (strchr ("/", a))
      b = 1;
  return b;
}

int
main (void)
{
  return 0;
}
