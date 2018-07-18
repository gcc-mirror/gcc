/* PR target/82386 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */
/* { dg-additional-options "-misel" { target powerpc*-*-* } } */

long long int fs;
int vm;

void
sd (void)
{
  fs = 1;
  vm = 2;
  goto zf;

  if (0)
    {
      int y6 = 0;
      int *uu = &y6;
      short int he;
      int of = 0;

 zf:
      for (;;)
      {
          he = of;
          if (he || (fs |= vm))
            {
              *uu = fs;
              fs += vm;
            }
          if (y6 == vm)
            fs |= he;
          he = y6 || fs;
          fs /= 0;
        }
    }
}
