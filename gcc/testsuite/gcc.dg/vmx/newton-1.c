/* { dg-do compile } */
#include <altivec.h>
                 
#define SPLAT76 ((vector unsigned char)\
                 {0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3})
#define SPLAT54 ((vector unsigned char)\
                 {4,5,6,7,4,5,6,7,4,5,6,7,4,5,6,7})
#define SPLAT32 ((vector unsigned char)\
                 {8,9,10,11,8,9,10,11,8,9,10,11,8,9,10,11})
#define SPLAT10 ((vector unsigned char)\
                 {12,13,14,15,12,13,14,15,12,13,14,15,12,13,14,15})
#define INTERLEAVE ((vector unsigned char)\
                    {0,1,16,17,4,5,20,21,8,9,24,25,12,13,28,29})

long real_32_manytaps (long ntaps, vector signed short *c, long ndat,
                       vector signed short *x, vector signed short *y)
{
  long i, j, op, ndatavec, ncoefvec;
  vector signed short x0, x1;
  vector signed short coef;
  vector signed short cr10, cr32, cr54, cr76;
  vector signed int y_even, y_odd;
  vector signed short *x1p;

  op = 0;
  ndatavec = ndat >> 3;
  ncoefvec = ntaps >> 3;

  for (i = 0; i < ndatavec; i += 1) {
    x0 = x[i];
        
    y_even = ((vector signed int){0x8000,0x8000,0x8000,0x8000});
    y_odd = ((vector signed int){0x8000,0x8000,0x8000,0x8000});
        
    j = 0;
    x1p = x + 1 + i;
      
    do {
 
      coef = c[j];
      x1 = x1p[j];
    
      cr10 = vec_perm(coef, coef, SPLAT10);
      y_odd = vec_msums(cr10, x1, y_odd);
      y_even = vec_msums(cr10, vec_sld(x0, x1, 14), y_even);

      cr32 = vec_perm(coef, coef, SPLAT32);
      y_odd = vec_msums(cr32, vec_sld(x0, x1, 12), y_odd);       
      y_even = vec_msums(cr32, vec_sld(x0, x1, 10), y_even);

      cr54 = vec_perm(coef, coef, SPLAT54);
      y_odd = vec_msums(cr54, vec_sld(x0, x1, 8), y_odd);        
      y_even = vec_msums(cr54, vec_sld(x0, x1, 6), y_even);

      cr76 = vec_perm(coef, coef, SPLAT76);
      y_odd = vec_msums(cr76, vec_sld(x0, x1, 4), y_odd);       
      y_even = vec_msums(cr76, vec_sld(x0, x1, 2), y_even);

      x0 = x1;

    } while (++j < ncoefvec);
    y[op++] = (vector signed short) vec_perm(y_even, y_odd, INTERLEAVE);

  }

  return op*8;
}
