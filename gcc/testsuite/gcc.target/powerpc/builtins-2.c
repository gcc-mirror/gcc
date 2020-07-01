/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 " } */

#include <altivec.h>

#ifdef DEBUG 
#include <stdio.h>
#endif

void abort (void);

int main ()
{
  vector int inta = {27, -1, 4, 9};
  vector unsigned int uinta = {9, 0, 7, 222};

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

  vector float fa = vec_ctf (inta, 5);
  if (fa[0] != 0.843750 || fa[1] != -0.031250 || fa[2] != 0.125000 || fa[3] != 0.281250)
#ifdef DEBUG
    printf("fa[0] = %f, fa[1] = %f, fa[2] = %f, fa[3] = %f\n",
           fa[0], fa[1], fa[2], fa[3]);
#else  
    abort ();
#endif

  vector int sa2 = vec_cts (fa, 5);
  if (sa2[0] != 27 || sa2[1] != -1 || sa2[2] != 4 || sa2[3] != 9)
#ifdef DEBUG
    printf("sa2[0] = %d, sa2[1] = %d, sa2[2] = %d, sa2[3] = %d\n",
           sa2[0], sa2[1], sa2[2], sa2[3]);
#else  
    abort ();
#endif
     
  vector float fb = vec_ctf (uinta, 2);

  if (fb[0] != 2.250000 || fb[1] != 0.0 || fb[2] != 1.750000 || fb[3] != 55.500000)
#ifdef DEBUG
    printf("fb[0] = %f, fb[1] = %f, fb[2] = %f, fb[3] = %f\n",
		 fb[0], fb[1], fb[2], fb[3]);
#else  
    abort ();
#endif

  vector unsigned int ua2 = vec_ctu (fb, 2);
  if (ua2[0] != 9 || ua2[1] != 0 || ua2[2] != 7 || ua2[3] != 222)
#ifdef DEBUG
    printf("ua2[0] = %d, ua2[1] = %d, ua2[2] = %d, ua2[3] = %d\n",
           ua2[0], ua2[1], ua2[2], ua2[3]);
#else  
    abort ();
#endif
     
  return 0;
}
