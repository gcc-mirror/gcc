/* PR target/96558 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-expensive-optimizations -fno-gcse" } */

int ky;
long int h1;
__int128 f1;

int
sd (void);

int __attribute__ ((simd))
i8 (void)
{
  __int128 vh;

  if (sd () == 0)
    h1 = 0;

  do
    {
      long int lf = (long int) f1 ? h1 : 0;

      ky += lf;
      vh = lf | f1;
      f1 = 1;
    }
  while (vh < (f1 ^ 2));

  return 0;
}

