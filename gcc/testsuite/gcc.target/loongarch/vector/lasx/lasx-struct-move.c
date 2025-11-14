/* { dg-do compile } */
/* { dg-options "-O2 -lasx" } */

struct vec
{
  int vec_a[32];
  double vec_b[5];
  char vec_c[32];
};

void
foo (struct vec *dest, struct vec *src, int index)
{
  dest[index] = *src;
}

/* { dg-final { scan-assembler-times "alsl\.d" 3} } */
