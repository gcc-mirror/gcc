/* PR target/88178 */
/* { dg-do compile } */
/* { dg-options "-g" } */

void foo (void)
{
  register int r19 asm ("19");
}
