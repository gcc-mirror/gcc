/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvdf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvdq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef int int32;
typedef long long int64;
typedef __int128 int128;

#define vd(T) \
void \
vd_##T (T *res, T *x, T *y) \
{ \
  for (int i = 0; i < 128; ++i) \
    res[i] = x[i] / *y; \
}

vd(int32)
vd(int64)
vd(int128)
