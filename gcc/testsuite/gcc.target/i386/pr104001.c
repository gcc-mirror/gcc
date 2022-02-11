/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "kandn" } } */
/* { dg-final { scan-assembler "andn" } } */

int b, c, d;
int r;

void
__attribute__((target("bmi")))
foo ()
{
  r = ((b & ~d) | (c & d));
}

void
__attribute__((target("avx512bw")))
bar ()
{
  r = ((b & ~d) | (c & d));
}
