/* { dg-do compile } */
/* { dg-options "-fno-sanitize=null -fsanitize=alignment -O2" } */
/* Check that when optimizing if we know the alignment is right
   and we are not doing -fsanitize=null instrumentation we don't
   instrument the alignment check.  */

__attribute__((noinline, noclone)) int
foo (char *p)
{
  p = (char *) __builtin_assume_aligned (p, __alignof__(int));
  int *q = (int *) p;
  return *q;
}

/* { dg-final { scan-assembler-not "__ubsan_handle" } } */
