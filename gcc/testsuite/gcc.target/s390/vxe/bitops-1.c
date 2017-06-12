/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=arch12 --save-temps" } */
/* { dg-require-effective-target s390_vxe } */

typedef unsigned int       uv4si __attribute__((vector_size(16)));

uv4si __attribute__((noinline))
not_xor (uv4si a, uv4si b)
{
  return ~(a ^ b);
}
/* { dg-final { scan-assembler-times "vnx\t%v24,%v24,%v26" 1 } } */

uv4si __attribute__((noinline))
not_and (uv4si a, uv4si b)
{
  return ~(a & b);
}
/* { dg-final { scan-assembler-times "vnn\t%v24,%v24,%v26" 1 } } */

uv4si __attribute__((noinline))
or_not (uv4si a, uv4si b)
{
  return a | ~b;
}
/* { dg-final { scan-assembler-times "voc\t%v24,%v24,%v26" 1 } } */


int
main ()
{
  uv4si a = (uv4si){ 42, 1, 0, 2 };
  uv4si b = (uv4si){ 42, 2, 0, 2 };
  uv4si c;

  c = not_xor (a, b);

  if (c[0] != ~0 || c[1] != ~3 || c[2] != ~0 || c[3] != ~0)
    __builtin_abort ();

  c = not_and (a, b);

  if (c[0] != ~42 || c[1] != ~0 || c[2] != ~0 || c[3] != ~2)
    __builtin_abort ();

  c = or_not (a, b);

  if (c[0] != ~0 || c[1] != ~2 || c[2] != ~0 || c[3] != ~0)
    __builtin_abort ();

  return 0;
}
