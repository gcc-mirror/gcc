/* PR ipa/59722 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

extern void abrt (const char *, int) __attribute__((noreturn));
void baz (int *, int *);

static inline int
bar (void)
{
  return 1;
}

static inline void
foo (int *x, int y (void))
{
  while (1)
    {
      int a = 0;
      if (*x)
	{
	  baz (x, &a);
	  while (a && !y ())
	    ;
	  break;
	}
      abrt ("", 1);
    }
}

void
test (int x)
{
  foo (&x, bar);
  foo (&x, bar);
}
