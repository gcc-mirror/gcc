/* { dg-do compile } */
/* { dg-options "-fno-sanitize=null -fsanitize=alignment -O2" } */
/* __builtin_assume_aligned should be instrumented too. UBSAN alignment
   should not depend on it.  */

__attribute__((noinline, noclone)) int
foo (char *p)
{
  p = (char *) __builtin_assume_aligned (p, __alignof__(int));
  int *q = (int *) p;
  return *q;
}

/* xfail, see PR 122038 as __builtin_assume_aligned should be instrumented instead
   of only the load. */
/* { dg-final { scan-assembler "__ubsan_handle" { xfail *-*-* } } } */
