/* PR debug/41717 */
/* { dg-do compile } */

void
foo (void)
{
  _Complex float v[1], w;
  v[1] = 0.0f + 0.8fi;
  w = __builtin_conjf (v[1] * v[1]);
}
