/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */
/* { dg-require-effective-target s390_vx } */

typedef unsigned char     uv16qi __attribute__((vector_size(16)));
typedef unsigned short     uv8hi __attribute__((vector_size(16)));
typedef unsigned int       uv4si __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));

uv2di __attribute__((noinline))
foo1 ()
{
  return (uv2di){ 0x7f0f, 0x7f0f };
}
/* { dg-final { scan-assembler-times "vrepig\t%v24,32527" 1 } } */

uv4si __attribute__((noinline))
foo2 ()
{
  return (uv4si){ 0x7f0f, 0x7f0f, 0x7f0f, 0x7f0f };
}
/* { dg-final { scan-assembler-times "vrepif\t%v24,32527" 1 } } */

uv8hi __attribute__((noinline))
foo3 ()
{
  return (uv8hi){ 0x7f0f, 0x7f0f, 0x7f0f, 0x7f0f,
      0x7f0f, 0x7f0f, 0x7f0f, 0x7f0f };
}
/* { dg-final { scan-assembler-times "vrepih\t%v24,32527" 1 } } */

uv16qi __attribute__((noinline))
foo4 ()
{
  return (uv16qi){ 0x77, 0x77, 0x77, 0x77,
      0x77, 0x77, 0x77, 0x77,
      0x77, 0x77, 0x77, 0x77,
      0x77, 0x77, 0x77, 0x77 };
}
/* { dg-final { scan-assembler-times "vrepib\t%v24,119" 1 } } */

int
main ()
{
  if (foo1()[1] != 0x7f0f)
    __builtin_abort ();

  if (foo2()[1] != 0x7f0f)
    __builtin_abort ();

  if (foo3()[1] != 0x7f0f)
    __builtin_abort ();

  if (foo4()[1] != 0x77)
    __builtin_abort ();

  return 0;
}
