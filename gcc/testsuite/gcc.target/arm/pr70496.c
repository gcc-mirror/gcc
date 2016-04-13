/* { dg-do assemble } */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-require-effective-target arm_thumb2_ok } */

int i;
void
main (void)
{
  __asm__ volatile (".arm");
  i = 0;
  __asm__ volatile ("\n cbz r0, 2f\n2:");
}
