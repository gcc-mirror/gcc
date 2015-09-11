/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */
/* { dg-require-effective-target int128 } */

typedef unsigned char     uv16qi __attribute__((vector_size(16)));
typedef unsigned short     uv8hi __attribute__((vector_size(16)));
typedef unsigned int       uv4si __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));
typedef unsigned __int128  uv1ti __attribute__((vector_size(16)));

/* The elements differ.  */
uv2di __attribute__((noinline))
foo1 ()
{
  return (uv2di){ 0x000fffffffffff00, 0x0000ffffffffff00 };
}

/* Non-contiguous bitmasks */

uv4si __attribute__((noinline))
foo2 ()
{
  return (uv4si){ 0xff00100f, 0xff00100f, 0xff00100f, 0xff00100f };
}

uv8hi __attribute__((noinline))
foo3a ()
{
  return (uv8hi){ 0xf700, 0xf700, 0xf700, 0xf700,
      0xf700, 0xf700, 0xf700, 0xf700 };
}

uv8hi __attribute__((noinline))
foo3b ()
{
  return (uv8hi){ 0x10ff, 0x10ff, 0x10ff, 0x10ff,
      0x10ff, 0x10ff, 0x10ff, 0x10ff };
}

uv16qi __attribute__((noinline))
foo4 ()
{
  return (uv16qi){ 0x82, 0x82, 0x82, 0x82,
      0x82, 0x82, 0x82, 0x82,
      0x82, 0x82, 0x82, 0x82,
      0x82, 0x82, 0x82, 0x82 };
}

/* We do not have vgmq.  */
uv1ti
foo5()
{
  return (uv1ti){ ((unsigned __int128)1 << 53) - 1 };
}
/* { dg-final { scan-assembler-not "vgm" } } */
