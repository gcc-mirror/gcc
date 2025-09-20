/* { dg-do compile } */
/* { dg-options "-fno-sanitize=null -fsanitize=alignment -O2" } */
/* Check that when optimizing if we know the alignment is right
   and we are not doing -fsanitize=null instrumentation we don't
   instrument the alignment check.  */

__attribute__((noinline, noclone)) int
foo (int *p)
{
  /* Align the pointer explictly. */
  __INTPTR_TYPE__ t = (__INTPTR_TYPE__)p;
  t &= ~0xf;
  p = (int*)t;

  return *p;
}

/* { dg-final { scan-assembler-not "__ubsan_handle" } } */
