/* { dg-do assemble } */
/* { dg-require-effective-target aarch64_asm_lse_ok } */
/* { dg-options "-march=armv8-a" } */

/* Make sure that the function header in assembly doesn't override
   user asm arch_extension directives.  */

__asm__ (".arch_extension lse");

void
foo (int i, int *v)
{
  register int w0 asm ("w0") = i;
  register int *x1 asm ("x1") = v;

  asm volatile (
  "\tstset   %w[i], %[v]\n"
  : [i] "+r" (w0), [v] "+Q" (v)
  : "r" (x1)
  : "x30");
}
