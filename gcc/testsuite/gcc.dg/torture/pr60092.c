/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-skip-if "No undefined weak" { hppa*-*-hpux* && { ! lp64 } } } */
/* { dg-additional-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } } */
/* { dg-additional-options "-Wl,-flat_namespace" { target *-*-darwin[89]* } } */
/* { dg-xfail-run-if "posix_memalign modifies first arg on error" { *-*-solaris2.11* } { "-O0" } } */

typedef __SIZE_TYPE__ size_t;
extern int posix_memalign(void **memptr, size_t alignment, size_t size) __attribute__((weak));
extern void abort(void);
int
main (void)
{
  void *p;
  int ret;

  if (!posix_memalign)
    return 0;

  p = (void *)&ret;
  ret = posix_memalign (&p, sizeof (void *), -1);
  if (p != (void *)&ret)
    abort ();
  return 0;
}
