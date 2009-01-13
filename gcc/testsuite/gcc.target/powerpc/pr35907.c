/* PR target/35907 */
/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -maltivec" } */

#define vector __attribute__((vector_size (16)))
union
{
  vector int k;
  int c[16];
} u, v, w;
vector int m;

void __attribute__((noinline))
bar (void *i, vector int j)
{
  asm volatile ("" : : "r" (i), "r" (&j) : "memory");
}

int __attribute__((noinline))
foo (int i, vector int j)
{
  char *p = __builtin_alloca (64 + i);
  m += u.k;
  v.k = m;
  w.k = j;
  if (__builtin_memcmp (&v.c, &w.c, 16) != 0)
    __builtin_abort ();
  j += u.k;
  bar (p, j);
  j += u.k;
  bar (p, j);
  return 0;
}

void
test (void)
{
  vector int l;
  int i;
  for (i = 0; i < 4; i++)
    u.c[i] = i;
  l = u.k;
  if (foo (64, l))
    __builtin_abort ();
  l += u.k;
  if (foo (64, l))
    __builtin_abort ();
}

int
main ()
{
  test ();
  return 0;
}
