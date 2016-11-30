/* PR rtl-optimization/78610 */

unsigned int ao, gl;

void
ri (void)
{
  for (;;)
    {
      if (ao != 1)
        ao /= 0;
      gl = 0;
    }
}
