/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */
/* { dg-require-effective-target s390_vx } */
/* { dg-require-effective-target int128 } */

typedef unsigned char     uv16qi __attribute__((vector_size(16)));
typedef unsigned short     uv8hi __attribute__((vector_size(16)));
typedef unsigned int       uv4si __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));
typedef unsigned __int128  uv1ti __attribute__((vector_size(16)));

uv2di __attribute__((noinline))
foo1 ()
{
  return (uv2di){ 0xff00ff00ff00ff00, 0x00ff00ff00ff00ff };
}
/* { dg-final { scan-assembler-times "vgbm\t%v24,43605" 1 } } */

uv4si __attribute__((noinline))
foo2 ()
{
  return (uv4si){ 0xff0000ff, 0x0000ffff, 0xffff0000, 0x00ffff00 };
}
/* { dg-final { scan-assembler-times "vgbm\t%v24,37830" 1 } } */

uv8hi __attribute__((noinline))
foo3a ()
{
  return (uv8hi){ 0xff00, 0xff00, 0xff00, 0xff00,
      0xff00, 0xff00, 0xff00, 0xff00 };
}
/* { dg-final { scan-assembler-times "vgbm\t%v24,43690" 1 } } */

uv8hi __attribute__((noinline))
foo3b ()
{
  return (uv8hi){ 0x00ff, 0x00ff, 0x00ff, 0x00ff,
      0x00ff, 0x00ff, 0x00ff, 0x00ff };
}
/* { dg-final { scan-assembler-times "vgbm\t%v24,21845" 1 } } */

uv16qi __attribute__((noinline))
foo4 ()
{
  return (uv16qi){ 0xff, 0xff, 0xff, 0xff,
      0, 0, 0, 0,
      0xff, 0, 0xff, 0,
      0, 0xff, 0, 0xff };
}

uv1ti __attribute__((noinline))
foo5 ()
{
  return (uv1ti){ 0xff00ff00ff00ff00ULL };
}

/* { dg-final { scan-assembler-times "vgbm\t%v24,61605" 1 } } */

int
main ()
{
  if (foo1()[1] != 0x00ff00ff00ff00ffULL)
    __builtin_abort ();

  if (foo2()[1] != 0x0000ffff)
    __builtin_abort ();

  if (foo3a()[1] != 0xff00)
    __builtin_abort ();

  if (foo3b()[1] != 0x00ff)
    __builtin_abort ();

  if (foo4()[1] != 0xff)
    __builtin_abort ();

  if (foo5()[0] != 0xff00ff00ff00ff00ULL)
    __builtin_abort ();

  return 0;
}

