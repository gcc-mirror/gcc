/* PR debug/41893 */
/* { dg-do compile } */

extern struct S s;

void
func2 (void)
{
  &s;
}
