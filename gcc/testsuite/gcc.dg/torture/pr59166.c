/* PR rtl-optimization/59166 */
/* { dg-additional-options "-fcompare-debug" } */
/* { dg-xfail-if "AIX compare debug" { powerpc-ibm-aix* } } */

int a, b, c, f, g;

void
foo ()
{
  for (; b; b++)
    for (; f; f = g)
      for (; a;)
	;
}

static int
bar (int p)
{
  short d;
  if (c)
    {
      for (; f; f = g);
      foo ();
      d = p;
      char e = d;
      if (p)
	return 1;
    }
  return p;
}

int
main ()
{
  bar (0);
  bar (g);
  return 0;
}
