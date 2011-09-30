/* PR inline-asm/50571 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

static const int var[4] = { 1, 2, 3, 4 };

void
foo (void)
{
  __asm volatile ("" : : "m" (*(int *) var));
}
