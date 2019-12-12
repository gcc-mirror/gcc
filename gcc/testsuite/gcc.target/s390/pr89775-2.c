/* PR target/89775 */
/* { dg-do compile } */

extern int foo (const char *, const char *);

__attribute__((noipa)) void
bar (const char *p)
{
  static const char *x;
  if (!x)
    x = p;
  else if (p != x)
    __builtin_abort ();
}

int
main ()
{
  char a[8] = "abcdefg";
  bar (a);
  if (foo (a, a) != 1)
    __builtin_abort ();
  bar (a);
  return 0;
}
