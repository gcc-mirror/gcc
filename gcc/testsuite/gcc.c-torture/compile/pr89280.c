// { dg-require-effective-target nonlocal_goto }
// { dg-require-effective-target label_values }
/* PR tree-optimization/89280 */

int a;
void foo (void);
__attribute__ ((returns_twice)) int bar (void);
void baz (int, int);
void *buf[5];

static inline void
inl (int x)
{
  while (x)
    foo ();
}

void
test1 (void)
{
  for (;;)
    foo ();
  baz (bar (), a);
}

void
test2 (void)
{
  for (;;)
    foo ();
  baz (__builtin_setjmp (buf), a);
  if (a)
    __builtin_longjmp (buf, 1);
}

void
test3 (void)
{
  inl (1);
  baz (bar (), a);
}

void
test4 (void)
{
  inl (2);
  baz (__builtin_setjmp (buf), a);
  if (a)
    __builtin_longjmp (buf, 1);
}
