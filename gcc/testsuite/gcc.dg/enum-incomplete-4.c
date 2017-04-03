/* PR c/79662 */
/* { dg-do compile } */
/* { dg-options "" } */

extern enum e ve;

int
f0 (int i)
{
  f0 (ve); /* { dg-error "incomplete" } */
}
