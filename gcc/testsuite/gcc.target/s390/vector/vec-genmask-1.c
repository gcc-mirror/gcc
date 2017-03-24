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
  return (uv2di){ 0x000fffffffffff00, 0x000fffffffffff00 };
}
/* { dg-final { scan-assembler-times "vgmg\t%v24,12,55" 1 } } */

uv4si __attribute__((noinline))
foo2 ()
{
  return (uv4si){ 0xff00000f, 0xff00000f, 0xff00000f, 0xff00000f };
}
/* { dg-final { scan-assembler-times "vgmf\t%v24,28,7" 1 } } */

uv8hi __attribute__((noinline))
foo3a ()
{
  return (uv8hi){ 0xfff0, 0xfff0, 0xfff0, 0xfff0,
      0xfff0, 0xfff0, 0xfff0, 0xfff0 };
}
/* { dg-final { scan-assembler-times "vgmh\t%v24,0,11" 1 } } */

uv8hi __attribute__((noinline))
foo3b ()
{
  return (uv8hi){ 0x0fff, 0x0fff, 0x0fff, 0x0fff,
      0x0fff, 0x0fff, 0x0fff, 0x0fff };
}
/* { dg-final { scan-assembler-times "vgmh\t%v24,4,15" 1 } } */

uv16qi __attribute__((noinline))
foo4 ()
{
  return (uv16qi){ 0x8, 0x8, 0x8, 0x8,
      0x8, 0x8, 0x8, 0x8,
      0x8, 0x8, 0x8, 0x8,
      0x8, 0x8, 0x8, 0x8 };
}
/* { dg-final { scan-assembler-times "vgmb\t%v24,4,4" 1 } } */

int
main ()
{
  if (foo1()[1] != 0x000fffffffffff00ULL)
    __builtin_abort ();

  if (foo2()[1] != 0xff00000f)
    __builtin_abort ();

  if (foo3a()[1] != 0xfff0)
    __builtin_abort ();

  if (foo3b()[1] != 0x0fff)
    __builtin_abort ();

  if (foo4()[1] != 0x8)
    __builtin_abort ();
  return 0;
}
