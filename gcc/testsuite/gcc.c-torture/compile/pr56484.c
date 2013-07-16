/* PR rtl-optimization/56484 */

unsigned char b[4096];
int bar (void);

int
foo (void)
{
  int a = 0;
  while (bar ())
    {
      int c = bar ();
      a = a < 0 ? a : c;
      __builtin_memset (b, 0, sizeof b);
    }
  return a;
}
