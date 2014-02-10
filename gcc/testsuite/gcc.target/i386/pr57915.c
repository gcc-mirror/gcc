/* PR rtl-optimization/57915 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

extern struct T { char a[8]; char b[16]; } t;
int c;
void foo (void);

extern inline char *
baz (char *x, const char *y)
{
  const char *e = y;
  unsigned long f, g;
  asm ("" : "+c" (f), "+D" (e) : "a" ('\0'), "X" (*e));
  g = e - 1 - y;
  __builtin_memcpy (x, y, g);
  x[g] = '\0';
  return x;
}

void
bar (void)
{
  char d[16];
  baz (d, t.b);

  for (;;)
    {
      foo ();
      if (c)
	baz (d, t.b);
    }
}
