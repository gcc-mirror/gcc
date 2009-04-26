/* PR c/39889 */
/* { dg-do compile } */
/* { dg-options "-Wunused-value" } */

int x;
int foo (void)
{
  return (1 ? x = 0 : (void) 0), 0; /* { dg-bogus "value computed is not used" } */
}
