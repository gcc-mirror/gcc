/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** test_1:
** sext\.w\s+[atx][0-9]+,\s*[atx][0-9]+
** ...
*/
void
test_1 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_2:
** ...
** vadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** ...
*/
void
__attribute__((target("arch=+v")))
test_2 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_3:
** ...
** minu\s+[atx][0-9]+,\s*[atx][0-9]+,\s*[atx][0-9]+
** ...
*/
void
__attribute__((target("arch=+zbb")))
test_3 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_4:
** sext\.w\s+[atx][0-9]+,\s*[atx][0-9]+
** ...
*/
void
test_4 (int *a, int *b, int *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_5:
** ...
** fadd\.h\s+fa[0-9]+,\s*fa[0-9]+,\s*fa[0-9]+
** ...
*/
void
__attribute__((target("arch=+zfh")))
test_5 (_Float16 *a, _Float16 *b, _Float16 *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/*
** test_6:
** ...
** call\s+__extendhfsf2
** ...
** call\s+__truncsfhf2
** ...
*/
void
test_6 (_Float16 *a, _Float16 *b, _Float16 *out, unsigned count)
{
  unsigned i;

  count = count > 128 ? 128 : count;

  for (i = 0; i < count; i++)
    out[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0\"" } } */
/* { dg-final { scan-assembler ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_v1p0_zicsr2p0_zifencei2p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0_zvl128b1p0_zvl32b1p0_zvl64b1p0" } } */
/* { dg-final { scan-assembler ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zbb1p0" } } */
/* { dg-final { scan-assembler ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zfh1p0_zfhmin1p0" } } */
