/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR target/20126 was not really target-specific, but rather a loop's
   failure to take into account the possibility that a DEST_ADDR giv
   replacement might fail, such as when you attempt to replace a REG
   with a PLUS in one of the register_operands of cmpstrqi_rex_1.  */

extern void abort (void);

typedef struct { int a; char b[3]; } S;
S c = { 2, "aa" }, d = { 2, "aa" };

void *
bar (const void *x, int y, int z)
{
  return (void *) 0;
}

int
foo (S *x, S *y)
{
  const char *e, *f, *g;
  int h;

  h = y->a;
  f = y->b;
  e = x->b;

  if (h == 1)
    return bar (e, *f, x->a) != 0;

  g = e + x->a - h;
  while (e <= g)
    {
      const char *t = e + 1;
      /* The pointer E below increases but the bound H stays constant,
	 letting the latter exceed the size remaining in the argument
	 pointed to by the formed, which might be detected by
	 -Wstringop-overread.  */
      if (__builtin_memcmp (e, f, h) == 0)
        return 1;
      e = t;
    }
  return 0;
}

int
main (void)
{
  if (foo (&c, &d) != 1)
    abort ();
  return 0;
}

/* { dg-prune-output "-Wstringop-overread" } */
