/* PR target/43636 */

extern char a[], *b[];

char *
foo (char *x, int y)
{
  x = __builtin_stpcpy (x, b[a[y]]);
  return x;
}
