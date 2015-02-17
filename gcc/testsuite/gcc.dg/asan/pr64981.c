/* PR sanitizer/64981 */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fsanitize=address -march=x86-64" } */

int
main ()
{
  __builtin_ia32_rdtsc ();
  return 0;
}

/* { dg-final { scan-assembler-not "__builtin_ia32_rdtsc" } } */
