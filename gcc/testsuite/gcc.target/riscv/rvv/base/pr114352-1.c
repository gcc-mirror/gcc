/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** test_1:
** beq\s+a3,\s*zero,\s*\.L[0-9]+
** ...
** bne\s+[atx][0-9]+,\s*[atx][0-9]+,\s*\.L[0-9]+
** ...
** ret
*/
void
test_1 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_2:
** ...
** vsetvli\s+[atx][0-9]+,\s*[atx][0-9]+,\s*e32,\s*m1,\s*ta,\s*ma
** ...
** vadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** ...
*/
void
__attribute__((target("arch=+v")))
test_2 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_3:
** beq\s+a3,\s*zero,\s*\.L[0-9]+
** ...
** bne\s+[atx][0-9]+,\s*[atx][0-9]+,\s*\.L[0-9]+
** ...
** ret
*/
void
test_3 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0\"" } } */
/* { dg-final { scan-assembler ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_v1p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0_zvl128b1p0_zvl32b1p0_zvl64b1p0" } } */
