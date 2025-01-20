/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler {\tvrf\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrg\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */
/* { dg-final { scan-assembler {\tvrq\t%v[0-9]+,%v[0-9]+,%v[0-9]+,0} } } */

typedef int int32;
typedef long long int64;
typedef __int128 int128;

#define vr(T) \
void \
vr_##T (T *res, T *x, T *y) \
{ \
  for (int i = 0; i < 128; ++i) \
    res[i] = x[i] % *y; \
}

vr(int32)
vr(int64)
vr(int128)
