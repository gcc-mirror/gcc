/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvdlf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdlg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdlq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef unsigned int uint32;
typedef unsigned long long uint64;
typedef unsigned __int128 uint128;

#define vdl(T) \
void \
vdl_##T (T *res, T *x, T *y) \
{ \
  for (int i = 0; i < 128; ++i) \
    res[i] = x[i] / *y; \
}

vdl(uint32)
vdl(uint64)
vdl(uint128)
