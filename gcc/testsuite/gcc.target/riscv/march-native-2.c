/* -mtune=native must not be rejected, whether or not the running core is
   one the compiler knows how to name.  */

/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-options "-mtune=native" } */

int
main (void)
{
  return 0;
}
