/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "" } */

void foo (void)
{
  asm volatile ("" : : : "%r12"); /* { dg-error "cannot be clobbered" } */
}
