/* PR c/47150 */

float _Complex foo (float, float);

void
bar ()
{
  float w = 2;
  float _Complex b;
  b = 0.5 * (foo (0, w) + foo (1, w) / w);
}
