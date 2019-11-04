/* PR tree-optimization/92056 */

const char *d;

void
foo (int c, char *e, const char *a, const char *b)
{
  switch (c)
    {
    case 33:
      for (;; d++)
        if (__builtin_strcmp (b ? : "", d))
          return;
      break;
    case 4:
      __builtin_sprintf (e, a);
    }
}
