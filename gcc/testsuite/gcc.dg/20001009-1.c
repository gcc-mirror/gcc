/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-warning "not supported" "PIC unsupported" { target cris-*-elf* cris-*-aout* mmix-*-* } 0 } */

extern void foo (void *a, double x, double y);
void
bar (void *a, double b, double c, double d, double e, double f, double g, double h, double i, double j, double k[6])
{
  double l, m, n, o, p;
  double q, r, s, t, u;
  double x, y, v, w;
  double z = 0.5 * j;
  double aa;
  l = b - 3 * d;
  m = 4 * f;
  n = f - h;
  q = c + 3 * g - i;
  r = 2 * (e - 2 * g + i);
  s = g - i;
  if (l != 0.0)
    {
      aa = 0.0;
      o = (- m + aa);
      p = (- m - aa);
      if (o > 0.0)
        {
          x = ((b-h)*o + 3*(d+h)*o + 3*(f-h)*o);
          y = ((c-i)*o + 3*(e+i)*o + 3*(g-i)*o);
          foo (a, z, w);
          foo (a, -z, w);
        }
      if (p > 0.0)
        {
          x = ((b+3*f-h)*p + 3*(d-2*f+h)*p + 3*p);
          y = ((c+3*g-i)*p + 3*(e-2*g+i)*p + 3*p);
          v = x * k[0] + y * k[2];
          w = x * k[1] + y * k[3];
          foo (a, z, w);
          foo (a, - z, w);
        }
    }
  if (q != 0.0)
    {
      aa = 0.0;
      t = (- r + aa) / (2 * q);
      u = (- r - aa) / (2 * q);
    }
}
