extern void exit (int);
extern void abort (void);
float x = -1.5f;

float
rintf ()
{
  static const float TWO23 = 8388608.0;

  if (__builtin_fabs (x) < TWO23)
    {
      if (x > 0.0)
        {
          x += TWO23;
          x -= TWO23;
        }
      else if (x < 0.0)
        {
          x = TWO23 - x;
          x = -(x - TWO23);
        }
    }

  return x;
}

int main (void)
{
  if (rintf () != -2.0)
    abort ();
  exit (0);
}
