/* PR  tree-optimizatiom/65178 - incorrect -Wmaybe-uninitialized when using
   nested loops
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void *bar (int);

char *foo (void)
{
  char *c = "bla";
  char *buf;
  for (int a = 1;; a = 0)
    {
      for (char *s = c; *s; ++s)
        {
        }
      if (!a) break;
      buf = (char *) bar (1);
    }
  return buf;       // { dg-bogus "\\\[-Wmaybe-uninitialized" }
}
