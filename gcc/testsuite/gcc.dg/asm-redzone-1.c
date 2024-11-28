/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  asm ("" : : : "cc", "memory", "redzone");
}
