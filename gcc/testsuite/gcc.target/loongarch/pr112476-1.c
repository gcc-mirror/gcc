/* PR target/112476: ICE with -mlsx */
/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mfpu=64 -mabi=lp64d -mlsx" } */

int foo, bar;
float baz, res, a;

void
apply_adjacent_ternary (float *dst, float *src0)
{
  do
    {
      __builtin_memcpy (&res, &src0, sizeof (res));
      *dst = foo ? baz : res;
      dst++;
    }
  while (dst != src0);
}

void
xx (void)
{
  apply_adjacent_ternary (&a, &a);
}
