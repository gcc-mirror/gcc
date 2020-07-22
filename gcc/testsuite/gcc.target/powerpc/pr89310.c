/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 " } */

struct s {
  int i;
  float f;
};

float
foo (struct s arg)
{
  return arg.f;
}

/* { dg-final { scan-assembler-not {\msrdi\M} } } */
/* { dg-final { scan-assembler-not {\msldi\M} {target le} } } */
/* { dg-final { scan-assembler-times {\mrldicr\M} 1 {target le} } } */
