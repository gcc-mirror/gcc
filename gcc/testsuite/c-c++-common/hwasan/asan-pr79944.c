/* PR sanitizer/79944 */
/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

struct S { int i; char p[1024]; };

int
main ()
{
  struct S *p = (struct S *) __builtin_malloc (__builtin_offsetof (struct S, p) + 64);
  p->i = 5;
  asm volatile ("" : "+r" (p) : : "memory");
  __atomic_fetch_add ((int *) p, 5, __ATOMIC_RELAXED);
  asm volatile ("" : "+r" (p) : : "memory");
  if (p->i != 10)
    __builtin_abort ();
  __builtin_free (p);
  return 0;
}
