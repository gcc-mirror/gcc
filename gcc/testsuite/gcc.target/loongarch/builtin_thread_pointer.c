/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "or\t\\\$r4,\\\$r2,\\\$r0" } } */

void *
get_tp ()
{
  return __builtin_thread_pointer ();
}
