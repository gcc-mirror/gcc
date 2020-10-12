/* PR tree-optimization/96758 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int v = 1;

int
main ()
{
  const char *s = v ? "a" : "b";
  char x[5];
  char y[5] = "a\0a";
  __builtin_memcpy (x, y, sizeof (y));
  if (__builtin_strncmp (x, s, 4) != 0)
    __builtin_abort ();
  return 0;
}
