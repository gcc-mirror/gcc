/* PR tree-optimization/33017 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize" } */

extern __SIZE_TYPE__ strlen (const char *);
extern void abort (void);

char *
__attribute__((noinline))
foo (const char *string)
{
  int len;
  static char var[0x104];
  int off;
  len = strlen (string);
  for (off = 0; off < 64; off++)
    var[len + off + 2] = 0x57;
  return var;
}

int
main (void)
{
  char *p = foo ("abcd");
  int i;
  for (i = 0; i < 0x104; i++)
    if (p[i] != ((i >= 6 && i < 70) ? 0x57 : 0))
      abort ();
  return 0;
}
