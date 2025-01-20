/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvrlf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrlg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrlq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef unsigned int uint32;
typedef unsigned long long uint64;
typedef unsigned __int128 uint128;

#define vrl(T) \
void \
vrl_##T (T *res, T *x, T *y) \
{ \
  for (int i = 0; i < 128; ++i) \
    res[i] = x[i] % *y; \
}

vrl(uint32)
vrl(uint64)
vrl(uint128)
