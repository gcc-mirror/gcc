/* { dg-do run { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 " } */

#include <altivec.h>

void abort (void);

int main ()
{
  vector long long sa = {27L, -14L};
  vector long long sb = {-9L, -2L};

  vector unsigned long long ua = {27L, 14L};
  vector unsigned long long ub = {9L, 2L};

  vector long long sc = vec_div (sa, sb);
  vector unsigned long long uc = vec_div (ua, ub);

  if (sc[0] != -3L || sc[1] != 7L || uc[0] != 3L || uc[1] != 7L)
    abort ();

  vector long long sd = vec_mul (sa, sb);
  vector unsigned long long ud = vec_mul (ua, ub);

  if (sd[0] != -243L || sd[1] != 28L || ud[0] != 243L || ud[1] != 28L)
    abort ();

  vector long long se = vec_splat (sa, 0);
  vector long long sf = vec_splat (sa, 1);
  vector unsigned long long ue = vec_splat (ua, 0);
  vector unsigned long long uf = vec_splat (ua, 1);

  if (se[0] != 27L || se[1] != 27L || sf[0] != -14L || sf[1] != -14L
      || ue[0] != 27L || ue[1] != 27L || uf[0] != 14L || uf[1] != 14L)
    abort ();

  vector double da = vec_ctf (sa, -2);
  vector double db = vec_ctf (ua, 2);
  vector long long sg = vec_cts (da, -2);
  vector unsigned long long ug = vec_ctu (db, 2);

  if (da[0] != 108.0 || da[1] != -56.0 || db[0] != 6.75 || db[1] != 3.5
      || sg[0] != 27L || sg[1] != -14L || ug[0] != 27L || ug[1] != 14L)
    abort ();

  return 0;
}
