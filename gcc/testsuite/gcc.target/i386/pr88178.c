/* PR target/88178 */
/* { dg-do compile } */
/* { dg-options "-g" } */

void foo (void)
{
  register int r19 asm ("19");	/* { dg-error "register specified for 'r19' is an internal GCC implementation detail" } */
}
