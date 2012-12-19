/* PR debug/55730 */
/* { dg-do compile } */
/* { dg-options "-w" } */

union U
{
  float f;
  int i;
};

void
foo (unsigned short *x, unsigned char y)
{
  unsigned char g;
  union U u;
  if (u.i < 0)
    g = 0;
  else
    {
      u.f = u.f * (255.0F / 256.0F) + 32768.0F;
      g = (unsigned char) u.i;
    }
  *x = (g << 8) | y;
}
