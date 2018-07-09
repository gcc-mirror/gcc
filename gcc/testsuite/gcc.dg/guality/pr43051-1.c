/* PR debug/43051 */
/* { dg-do run } */
/* { dg-options "-g" } */

extern void abort (void);

static void __attribute__ ((noinline))
foo (const char *x, long long y, int z)
{
  asm volatile ("" : : "r" (x), "r" ((int) y), "r" (z) : "memory");
}

struct S
{
  struct S *n;
  int v;
};

struct S a[10];

struct S * __attribute__ ((noinline))
bar (struct S *c, int v, struct S *e)
{
#ifdef __i386__
  register int si asm ("esi"), di asm ("edi"), bx
# if !defined (__pic__) && !defined (__APPLE__)
    asm ("ebx")
# endif
    ;
  asm volatile ("" : "=r" (si), "=r" (di), "=r" (bx));
#endif
  while (c < e)
    {
      foo ("c", (__UINTPTR_TYPE__) c, 0);	/* { dg-final { gdb-test . "c" "\&a\[0\]" } } */
      foo ("v", v, 1);				/* { dg-final { gdb-test . "v" "1" } } */
      foo ("e", (__UINTPTR_TYPE__) e, 2);	/* { dg-final { gdb-test . "e" "\&a\[1\]" } } */
      if (c->v == v)
	return c;
      foo ("c", (__UINTPTR_TYPE__) c, 3);	/* { dg-final { gdb-test . "c" "\&a\[0\]" } } */
      foo ("v", v, 4);				/* { dg-final { gdb-test . "v" "1" } } */
      foo ("e", (__UINTPTR_TYPE__) e, 5);	/* { dg-final { gdb-test . "e" "\&a\[1\]" } } */
      c++;
    }
#ifdef __i386__
  asm volatile ("" : : "r" (si), "r" (di), "r" (bx));
#endif
  return 0;
}

int
main ()
{
  asm volatile ("" : : "r" (&a[0]) : "memory");
  if (bar (&a[a[0].v], a[0].v + 1, &a[a[0].v + 1]))
    abort ();
  return 0;
}
