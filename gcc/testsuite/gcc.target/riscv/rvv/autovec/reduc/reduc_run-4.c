/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#include "reduc-4.c"

#define N 0x1100

int
main (void)
{
  unsigned short x[N];
  for (int i = 0; i < N; ++i)
    x[i] = (i + 1) * (i + 2);

  if (add_loop (x, 0, 10) != 10
      || add_loop (x, 11, 42) != 614
      || add_loop (x, 0x100, 84) != 22100
      || add_loop (x, 0xfff, 20) != 20500
      || max_loop (x, 0, 10) != 10
      || max_loop (x, 11, 131) != 132
      || max_loop (x, 11, 133) != 133
      || max_loop (x, 0x100, 65279) != 65280
      || max_loop (x, 0x100, 65281) != 65281
      || max_loop (x, 0xfff, 65503) != 65504
      || max_loop (x, 0xfff, 65505) != 65505
      || or_loop (x, 0, 0x71) != 0x71
      || or_loop (x, 11, 0) != 0xfe
      || or_loop (x, 11, 0xb3c) != 0xbfe
      || or_loop (x, 0x80, 0) != 0x7ffe
      || or_loop (x, 0x80, 1) != 0x7fff
      || or_loop (x, 0xb4, 0) != 0x7ffe
      || or_loop (x, 0xb4, 1) != 0x7fff
      || or_loop (x, 0xb5, 0) != 0xfffe
      || or_loop (x, 0xb5, 1) != 0xffff
      || eor_loop (x, 0, 0x3e) != 0x3e
      || eor_loop (x, 11, 0) != 0xe8
      || eor_loop (x, 11, 0x1ff) != 0x117
      || eor_loop (x, 0x100, 0) != 0xcf00
      || eor_loop (x, 0x100, 0xeee) != 0xc1ee
      || eor_loop (x, 0xfff, 0) != 0xa000
      || eor_loop (x, 0xfff, 0x8888) != 0x2888)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    x[i] = ~x[i];

  if (min_loop (x, 0, 10000) != 10000
      || min_loop (x, 11, 65404) != 65403
      || min_loop (x, 11, 65402) != 65402
      || min_loop (x, 0x100, 256) != 255
      || min_loop (x, 0x100, 254) != 254
      || min_loop (x, 0xfff, 32) != 31
      || min_loop (x, 0xfff, 30) != 30
      || and_loop (x, 0, 0x1234) != 0x1234
      || and_loop (x, 11, 0xffff) != 0xff01
      || and_loop (x, 11, 0xcdef) != 0xcd01
      || and_loop (x, 0x80, 0xffff) != 0x8001
      || and_loop (x, 0x80, 0xfffe) != 0x8000
      || and_loop (x, 0xb4, 0xffff) != 0x8001
      || and_loop (x, 0xb4, 0xfffe) != 0x8000
      || and_loop (x, 0xb5, 0xffff) != 1
      || and_loop (x, 0xb5, 0xfffe) != 0)
    __builtin_abort ();

  return 0;
}
