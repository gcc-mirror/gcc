/* PR 28875 */
/* { dg-do compile } */
/* { dg-options "-O3 -Wextra -Wno-unused-parameter -Wall" } */
static int t(int i) /* { dg-bogus "unused parameter" "unused parameter warning" } */
{
  return 0;
}
int tt()
{
  return t(0);
}
