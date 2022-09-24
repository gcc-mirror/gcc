/* { dg-do run } */
/* { dg-options "-O2 -fsignaling-nans" } */

#if __LDBL_MANT_DIG__ == 64
union U { struct { unsigned long long m; unsigned short e; } p; long double l; };
union U zero = { { 0, 0 } };
union U mzero = { { 0, 0x8000 } };
union U denorm = { { 42, 0 } };
union U mdenorm = { { 42, 0x8000 } };
union U pseudodenorm = { { 0x8000000000000000ULL, 0 } };
union U mpseudodenorm = { { 0x8000000000000000ULL, 0x8000 } };
union U pseudodenorm1 = { { 0x8000000000000042ULL, 0 } };
union U mpseudodenorm1 = { { 0x8000000000000042ULL, 0x8000 } };
union U pseudoinf = { { 0, 0x7fff } };
union U mpseudoinf = { { 0, 0xffff } };
union U pseudonan = { { 42, 0x7fff } };
union U mpseudonan = { { 42, 0xffff } };
union U pseudonan1 = { { 0x4000000000000000ULL, 0x7fff } };
union U mpseudonan1 = { { 0x4000000000000000ULL, 0xffff } };
union U pseudonan2 = { { 0x4000000000000042ULL, 0x7fff } };
union U mpseudonan2 = { { 0x4000000000000042ULL, 0xffff } };
union U inf = { { 0x8000000000000000ULL, 0x7fff } };
union U minf = { { 0x8000000000000000ULL, 0xffff } };
union U snan = { { 0x8000000000000042ULL, 0x7fff } };
union U msnan = { { 0x8000000000000042ULL, 0xffff } };
union U indefinite = { { 0xc000000000000000ULL, 0x7fff } };
union U mindefinite = { { 0xc000000000000000ULL, 0xffff } };
union U qnan = { { 0xc000000000000042ULL, 0x7fff } };
union U mqnan = { { 0xc000000000000042ULL, 0xffff } };
union U unnormal = { { 0, 0x42 } };
union U munnormal = { { 0, 0x8042 } };
union U unnormal1 = { { 42, 0x42 } };
union U munnormal1 = { { 42, 0x8042 } };
union U normal = { { 0x8000000000000000ULL, 0x42 } };
union U mnormal = { { 0x8000000000000000ULL, 0x8042 } };
union U normal1 = { { 0x8000000000000042ULL, 0x42 } };
union U mnormal1 = { { 0x8000000000000042ULL, 0x8042 } };
#endif

int
main ()
{
#if __LDBL_MANT_DIG__ == 64
  asm volatile ("" : : : "memory");
  if (__builtin_issignaling (zero.l)
      || __builtin_issignaling (mzero.l)
      || __builtin_issignaling (denorm.l)
      || __builtin_issignaling (mdenorm.l)
      || __builtin_issignaling (pseudodenorm.l)
      || __builtin_issignaling (mpseudodenorm.l)
      || __builtin_issignaling (pseudodenorm1.l)
      || __builtin_issignaling (mpseudodenorm1.l)
      || !__builtin_issignaling (pseudoinf.l)
      || !__builtin_issignaling (mpseudoinf.l)
      || !__builtin_issignaling (pseudonan.l)
      || !__builtin_issignaling (mpseudonan.l)
      || !__builtin_issignaling (pseudonan1.l)
      || !__builtin_issignaling (mpseudonan1.l)
      || !__builtin_issignaling (pseudonan2.l)
      || !__builtin_issignaling (mpseudonan2.l)
      || __builtin_issignaling (inf.l)
      || __builtin_issignaling (minf.l)
      || !__builtin_issignaling (snan.l)
      || !__builtin_issignaling (msnan.l)
      || __builtin_issignaling (indefinite.l)
      || __builtin_issignaling (mindefinite.l)
      || __builtin_issignaling (qnan.l)
      || __builtin_issignaling (mqnan.l)
      || !__builtin_issignaling (unnormal.l)
      || !__builtin_issignaling (munnormal.l)
      || !__builtin_issignaling (unnormal1.l)
      || !__builtin_issignaling (munnormal1.l)
      || __builtin_issignaling (normal.l)
      || __builtin_issignaling (mnormal.l)
      || __builtin_issignaling (normal1.l)
      || __builtin_issignaling (mnormal1.l))
    __builtin_abort ();
#endif
  return 0;
}
