/* PR debug/43557 */
/* { dg-do compile } */

extern struct S g;

void
f2 (void)
{
  &g;
}
