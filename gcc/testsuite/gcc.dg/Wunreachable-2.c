/* { dg-do compile } */
/* { dg-options "-O2 -Wunreachable-code" } */

extern int foo (const char *);
extern void baz (void);
const char *a[] = { "one", "two" };

void bar (void)
{
  int i;

  for (i = 0; i < 2; i++)
    if (! foo (a[i]))
      return;

  baz ();	/* { dg-bogus "will never be executed" } */
  baz ();
  baz ();
}
