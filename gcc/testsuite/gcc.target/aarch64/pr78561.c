/* { dg-do compile } */
/* { dg-options "-Og -O3 -mcmodel=tiny" } */

int
main (__fp16 x)
{
  __fp16 a = 6.5504e4;
  return (x <= a);
}
