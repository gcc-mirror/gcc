/* Verify we don't ICE on statement-expressions.  */
/* { dg-do compile } */

void foo(void)
{
  char buf[({ 4; })];
}
